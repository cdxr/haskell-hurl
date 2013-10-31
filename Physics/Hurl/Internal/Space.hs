module Physics.Hurl.Internal.Space (
    -- * Space
    Space,
    hipmunkSpace,
    newSpace,
    deleteSpace,
    stepSpace,

    -- * Hipmunk Initialization
    initializeChipmunk,
    debugChipmunkInitialized,

    -- * Objects
    -- WARNING: The garbage collector will not automatically remove the
    -- internal Chipmunk entities when it collects the Object. The user is
    -- responsible for preventing memory leaks by calling deleteObject on
    -- Objects.
    Object   (..),
    objectProto,
    SolidRef (..),
    
    -- ** Mobility
    Mobility (..),
    DynamicObject (..),
    dynamicObject,
    dynamicBody,

    -- ** Creation and Deletion
    addObject,
    addDynamic,
    addStatic,
    deleteObject,
    ) where

import Linear.V2

import Control.Lens ( view, Prism', prism' )
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

-- | Initialize Chipmunk. This may be safely called multiple times.
initializeChipmunk :: IO ()
initializeChipmunk = 
    -- check the global MVar to see if Chipmunk has been initialized
    modifyMVarMasked_ _chipmunkInitialized $ \b -> do
        unless b H.initChipmunk
        return True

-- | Check to see if Chipmunk has been initialized.
--
-- This operation is subject to race conditions. Specifically, it may
-- result in false negatives in the presence of concurrency.
--
debugChipmunkInitialized :: IO Bool
debugChipmunkInitialized =
    modifyMVar _chipmunkInitialized $ \b -> return (b, b)


-- | A `Space` is a mutable container of bodies, shapes, and constraints
-- that simulates their physical interactions.
newtype Space = Space { hipmunkSpace :: H.Space }
    deriving (Eq, Ord)


-- | Create a new empty `Space`.
newSpace :: IO Space
newSpace = do
    initializeChipmunk
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
-- Objects
------------------------------------------------------------------------------

-- | A reference to a `Solid` in a `Space`.
data SolidRef = SolidRef
    { solidRefProto :: Solid
    , solidRefShape :: H.Shape
    }

makeSolidRef :: H.Body -> Solid -> IO SolidRef
makeSolidRef b s = do
    hShape <- makeHipmunkShape (view shape s) b
    H.elasticity hShape $= view (surface.elasticity) s
    H.friction   hShape $= view (surface.friction)   s
    return $ SolidRef s hShape


-- | A reference to a collection of `Solid`s in a `Space`.
data Object f = Object
    { objectSpace     :: Space       -- ^ the Space
    , objectBody      :: H.Body      -- ^ the Hipmunk Body
    , objectSolids    :: f SolidRef  -- ^ the collection of SolidRefs
    , objectMobility  :: Mobility    -- ^ the mobility type
    , objectFinalizer :: IO ()       -- ^ remove from the space
    }

-- TODO: maybe it is better to store this rather than recreate it?

-- @objectProto o@ is the original container of `Solid`s that produced the
-- `Object`.
objectProto :: (Functor f) => Object f -> f Solid
objectProto = fmap solidRefProto . objectSolids


data Mobility = Static | Dynamic
    deriving (Show, Eq, Ord)


-- | A wrapper for an `Object` that is known to be dynamic, i.e. is mobile
-- and has physical properties such as mass and moment.
newtype DynamicObject f = DynamicObject { unDynamicObject :: Object f }

dynamicObject :: Prism' (Object f) (DynamicObject f)
dynamicObject = prism' unDynamicObject $ \o ->
    case objectMobility o of
        Dynamic -> Just $ DynamicObject o
        Static  -> Nothing

dynamicBody :: DynamicObject f -> H.Body
dynamicBody = objectBody . unDynamicObject


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


-- | Add a collection of `Solid`s to the space at the given position.
-- If the given `Mobility` is `Dynamic`, the resulting `Object` has a
-- mobile body with physical properties, and if it is `Static` the `Object`
-- is immobile.
-- 
-- The `Object` returned is a reference to the newly created Chipmunk entities
-- in the space.
--
addObject
    :: (Traversable f)
    => Mobility
    -> V2 Double
    -> f Solid
    -> Space
    -> IO (Object f)
addObject mob p solids space = do
    (hBody, makeRes) <- case mob of
        Static  -> createStaticBody
        Dynamic -> createDynamicBody ma mo
    H.position hBody $= vectorFromV2 p

    solidRefs <- Traversable.mapM (makeSolidRef hBody) solids

    let resources = makeRes . map solidRefShape . toList $ solidRefs
    delete <- runResource resources space

    return $ Object space hBody solidRefs mob delete
  where
    (ma, mo) = foldMap ((,) <$> view mass <*> moment) . toList $ solids


-- | Creates an `Object` like @addObject Dynamic@, but returning that
-- `Object` wrapped in a `DynamicObject`.
addDynamic :: (Traversable f) => V2 Double -> f Solid -> Space -> IO (DynamicObject f)
addDynamic p ss = fmap DynamicObject . addObject Dynamic p ss


-- | A synonym for @addObject Static@
addStatic :: (Traversable f) => V2 Double -> f Solid -> Space -> IO (Object f)
addStatic = addObject Static



-- | Delete all Chipmunk entities referenced by an `Object`.
--
-- WARNING: All operations on an Object that has been deleted are
-- undefined.
deleteObject :: Object f -> IO ()
deleteObject = objectFinalizer
