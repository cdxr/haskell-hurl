module Physics.Hurl.Internal.Space (
    Space,
    hipmunkSpace,
    newSpace,
    deleteSpace,
    stepSpace,
    spaceResource,
    ) where


import Control.Applicative
import Control.Monad ( unless )

import Control.Concurrent.MVar

import System.IO.Unsafe ( unsafePerformIO )

import qualified Physics.Hipmunk as H

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
