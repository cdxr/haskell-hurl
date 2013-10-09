module Physics.Hurl.Internal.Space (
    -- * Space
    Space,
    hipmunkSpace,
    newSpace,
    deleteSpace,
    stepSpace,
    -- * ObjectRefs
    ObjectRef (..),
    SolidRef,
    addObject,
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

type SolidRef = H.Shape


-- | A reference to a physical entity in a `Space`.
data ObjectRef f = ObjectRef
    { objectProto     :: Object f  -- ^ the original Object
    , objectSpace     :: Space
    , objectBody      :: H.Body
    , objectSolids    :: f SolidRef
    , objectFinalizer :: IO ()
    }


-- | Create a new Hipmunk Body and a function to create a resource from
-- Shapes.
createBody :: Body -> IO (H.Body, [H.Shape] -> Resource Space)
createBody body = case body of
    Static     -> do
        b <- H.newBody H.infinity H.infinity
        -- apparently in Hipmunk you shouldn't add a static body to a space,
        -- so here we ignore the body and only add the shapes
        return (b, foldMap $ spaceResource . H.Static)
    Body ma mo -> do
        b <- H.newBody ma mo
        return (b, \ss -> spaceResource b <> foldMap spaceResource ss)


-- | Allocate an @Object f@ in the space. The `ObjectRef` returned is a
-- reference to the newly created Chipmunk entities in the space.
--
-- WARNING: The garbage collector will not automatically remove the
-- internal Chipmunk entities when it collects the ObjectRef. The user is
-- responsible for preventing memory leaks by calling deleteObject on
-- ObjectRefs.
addObject :: (Traversable f) => V2 Double -> Object f -> Space -> IO (ObjectRef f)
addObject (V2 x y) o@(Object body solids) space = do
    (b, makeRes) <- createBody body
    H.position b $= H.Vector x y

    shapes <- Traversable.forM solids $ \(pos, s) -> do
        ref <- H.newShape b (shape s) (H.Vector x y)
        H.elasticity ref $= elasticity s
        H.friction   ref $= friction   s
        return ref

    ObjectRef o space b shapes <$> runResource (makeRes $ toList shapes) space


-- | Delete all Chipmunk entities referenced by an `ObjectRef`.
--
-- WARNING: All operations on an ObjectRef that has been deleted are
-- undefined.
deleteObject :: ObjectRef f -> IO ()
deleteObject = objectFinalizer
