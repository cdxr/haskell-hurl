module Physics.Hurl.Internal.Space (
    -- * Space
    Space,
    hipmunkSpace,
    newSpace,
    deleteSpace,
    stepSpace,
    -- * ObjectRefs
    ObjectRef (..),
    SolidRef (..),
    addObject,
    addStatics,
    deleteObject,
    ) where

import Linear.V2

import Control.Applicative
import Control.Monad ( unless )

import Data.Monoid
import Data.Foldable    ( Foldable, foldMap, toList )
import Data.Traversable ( Traversable )
import qualified Data.Traversable as Traversable

import Data.StateVar

import Control.Concurrent.MVar
import System.IO.Unsafe ( unsafePerformIO )

import qualified Physics.Hipmunk as H

import Physics.Hurl.Object
import Physics.Hurl.Solid

import Physics.Hurl.Internal.Resource
import Physics.Hurl.Internal.Shape


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

-- | A reference to a `Solid` in a `Space`.
data SolidRef = SolidRef
    { solidProto :: Solid
    , solidShape :: H.Shape
    }

-- | A reference to an `Object` in a `Space`.
data ObjectRef f = ObjectRef
    { objectProto     :: Either (f Solid) (Object f)
      -- ^ the original Object, or static Solids 
    , objectSpace     :: Space       -- ^ the Space
    , objectBody      :: H.Body      -- ^ the Hipmunk Body
    , objectSolids    :: f SolidRef  -- ^ the collection of Solids
    , objectFinalizer :: IO ()       -- ^ remove from the space
    }


-- | Create a new Hipmunk Body and a function to create a resource from
-- Shapes.
createBody :: Maybe Body -> IO (H.Body, [H.Shape] -> Resource Space)
createBody mbody = case mbody of
    Nothing -> do
        b <- H.newBody H.infinity H.infinity
        -- apparently in Hipmunk you shouldn't add a static body to a space,
        -- so here we ignore the body and only add the shapes
        return (b, foldMap $ spaceResource . H.Static)
    Just (Body ma mo) -> do
        b <- H.newBody ma mo
        return (b, \ss -> spaceResource b <> foldMap spaceResource ss)


addToSpace :: (Traversable f)
           => V2 Double
           -> Either (f Solid) (Object f)
           -> Space
           -> IO (ObjectRef f)
addToSpace (V2 x y) eso space = do
    (hBody, makeRes) <- createBody mbody
    H.position hBody $= H.Vector x y

    solidRefs <- Traversable.forM solids $ \s -> do
        hShape <- makeHipmunkShape (shape s) hBody
        H.elasticity hShape $= elasticity s
        H.friction   hShape $= friction   s
        return $ SolidRef s hShape

    delete <- runResource (makeRes . map solidShape $ toList solidRefs) space

    return $ ObjectRef eso space hBody solidRefs delete
  where
    (mbody, solids) = case eso of
        Left ss             -> (Nothing, ss)
        Right (Object b ss) -> (Just b, ss)


-- | Add an @Object f@ to the space. The `ObjectRef` returned is a reference
-- to the newly created Chipmunk entities in the space.
--
-- WARNING: The garbage collector will not automatically remove the
-- internal Chipmunk entities when it collects the ObjectRef. The user is
-- responsible for preventing memory leaks by calling deleteObject on
-- ObjectRefs.
addObject :: (Traversable f) => V2 Double -> Object f -> Space -> IO (ObjectRef f)
addObject p = addToSpace p . Right


-- | Add a collection of static `Solid`s to the space. The `ObjectRef` returned
-- is a reference to the newly created Chipmunk entities in the space.
--
-- WARNING: The garbage collector will not automatically remove the
-- internal Chipmunk entities when it collects the ObjectRef. The user is
-- responsible for preventing memory leaks by calling deleteObject on
-- ObjectRefs.
addStatics :: (Traversable f) => V2 Double -> f Solid -> Space -> IO (ObjectRef f)
addStatics p = addToSpace p . Left


-- | Delete all Chipmunk entities referenced by an `ObjectRef`.
--
-- WARNING: All operations on an ObjectRef that has been deleted are
-- undefined.
deleteObject :: ObjectRef f -> IO ()
deleteObject = objectFinalizer
