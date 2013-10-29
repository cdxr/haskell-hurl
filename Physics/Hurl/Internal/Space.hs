{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}


module Physics.Hurl.Internal.Space (
    -- * Space
    Space,
    hipmunkSpace,
    newSpace,
    deleteSpace,
    stepSpace,
    -- * ObjectRefs
    -- WARNING: The garbage collector will not automatically remove the
    -- internal Chipmunk entities when it collects the ObjectRef. The user is
    -- responsible for preventing memory leaks by calling deleteObject on
    -- ObjectRefs.
    ObjectRef (..),
    ObjectType (..),
    SolidRef (..),
    _addObject,
    addDynamic,
    addStatic,
    deleteObject,
    ) where

import Linear.V2

import Control.Lens ( view )
import Control.Applicative
import Control.Monad

import Data.Monoid
import Data.Foldable    ( Foldable, foldMap, toList )
import Data.Traversable ( Traversable )
import qualified Data.Traversable as Traversable

import Data.StateVar

import Control.Concurrent.MVar
import System.IO.Unsafe ( unsafePerformIO )

import qualified Physics.Hipmunk as H

--import Physics.Hurl.Object
import Physics.Hurl.Solid

import Physics.Hurl.Internal.Resource
import Physics.Hurl.Internal.Shape
import Physics.Hurl.Internal.Utils


-- TODO: combine `createBody` and `addToSpace`


-- | A global reference used to determine if we have called `H.initChipmunk`.
--
-- The first time we initialize a `Space`, we call `H.initChipmunk` and
-- set this to @True@.
_chipmunkInitialized :: MVar Bool
_chipmunkInitialized = unsafePerformIO $ newMVar False
{-# NOINLINE _chipmunkInitialized #-}


-- | A `Space` is a mutable container of bodies, shapes, and constraints
-- that simulates their physical interactions.
newtype Space = Space { hipmunkSpace :: H.Space }
    deriving (Eq, Ord)


-- | Create a new empty `Space`.
newSpace :: IO Space
newSpace = do
    -- check the global MVar to see if Chipmunk has been initialized
    modifyMVar_ _chipmunkInitialized $ \b -> do
        unless b H.initChipmunk
        return True
    Space <$> H.newSpace


-- todo: add a finalizer to perform this automatically

-- | Delete a `Space`. All operations on a deleted `Space` are undefined.
deleteSpace :: Space -> IO ()
deleteSpace = H.freeSpace . hipmunkSpace

-- | @stepSpace delta space@ will simulate @space@ for a @delta@ timestep.
-- It is necessary to perform this simulation at a fixed @delta@ to ensure
-- accuracy.
stepSpace :: Double -> Space -> IO ()
stepSpace delta space = H.step (hipmunkSpace space) (realToFrac delta)


-- | Create a 'Resource' for the given 'H.Entity'.
spaceResource :: (H.Entity e) => e -> Resource Space
spaceResource e = Resource $ \(Space s) -> (H.spaceAdd s e, H.spaceRemove s e)


------------------------------------------------------------------------------
-- ObjectRefs
------------------------------------------------------------------------------

data ObjectType = Static | Dynamic
    deriving (Show, Read, Eq, Ord, Bounded, Enum)


-- | A reference to a `Solid` in a `Space`.
data SolidRef = SolidRef
    { solidRefProto :: Solid
    , solidRefShape :: H.Shape
    }

-- | A reference to an `Object` in a `Space`.
data ObjectRef f (t :: ObjectType) = ObjectRef
    { objectSpace     :: Space       -- ^ the Space
    , objectBody      :: H.Body      -- ^ the Hipmunk Body
    , objectSolids    :: f SolidRef  -- ^ the collection of SolidRefs
    , objectFinalizer :: IO ()       -- ^ remove from the space
    }

objectProto :: (Functor f) => ObjectRef f t -> f Solid
objectProto = fmap solidRefProto . objectSolids


-- | Convenience function: move the object to the given position, returning
-- the same ObjectRef
--moveTo :: V2 Double -> ObjectRef t f -> IO (ObjectRef f)
--moveTo (V2 x y) o = o <$ (H.position (objectBody o) $= H.Vector x y)


createDynamicBody :: Mass -> Moment -> IO (H.Body, [H.Shape] -> Resource Space)
createDynamicBody (Mass ma) (Moment mo) = do
    b <- H.newBody ma mo
    return (b, \ss -> spaceResource b <> foldMap spaceResource ss)


createStaticBody :: IO (H.Body, [H.Shape] -> Resource Space)
createStaticBody = do
    b <- H.newBody H.infinity H.infinity
    -- apparently in Hipmunk you shouldn't add a static body to a space,
    -- so here we ignore the body and only add the shapes
    return (b, foldMap $ spaceResource . H.Static)


_addObject
    :: (Traversable f)
    => ObjectType
    -> V2 Double
    -> f Solid
    -> Space
    -> IO (ObjectRef f t)
_addObject ot p solids space = do
    (hBody, makeRes) <- case ot of
        Static  -> createStaticBody
        Dynamic -> createDynamicBody ma mo
    H.position hBody $= vectorFromV2 p

    solidRefs <- makeSolidRefs hBody solids

    let resources = makeRes . map solidRefShape . toList $ solidRefs
    delete <- runResource resources space

    return $ ObjectRef space hBody solidRefs delete
  where
    (ma, mo) = foldMap ((,) <$> view mass <*> moment) . toList $ solids


addDynamic :: (Traversable f) =>
    V2 Double -> f Solid -> Space -> IO (ObjectRef f Dynamic)
addDynamic = _addObject Dynamic


-- | Add a collection of static `Solid`s to the space. The `ObjectRef` returned
-- is a reference to the newly created Chipmunk entities in the space.
addStatic  :: (Traversable f) =>
    V2 Double -> f Solid -> Space -> IO (ObjectRef f Static)
addStatic = _addObject Static


makeSolidRefs :: (Traversable f) => H.Body -> f Solid -> IO (f SolidRef)
makeSolidRefs b = Traversable.mapM $ \(s :: Solid) -> do
    hShape <- makeHipmunkShape (view shape s) b
    H.elasticity hShape $= view (surface.elasticity) s
    H.friction   hShape $= view (surface.friction)   s
    return $ SolidRef s hShape


-- | Delete all Chipmunk entities referenced by an `ObjectRef`.
--
-- WARNING: All operations on an ObjectRef that has been deleted are
-- undefined.
deleteObject :: ObjectRef f t -> IO ()
deleteObject = objectFinalizer
