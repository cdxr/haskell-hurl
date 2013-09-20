module Physics.Hurl.Internal.Space where


import Control.Applicative
import Control.Monad ( unless )
import Data.Function ( on )

import Data.IORef
import System.IO.Unsafe ( unsafePerformIO )

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.Resource


-- | A global reference used to determine if we have called `H.initChipmunk`.
--
-- The first time we initialize a `Space`, we call `H.initChipmunk` and
-- set this to @True@.
_chipmunkInitialized :: IORef Bool
_chipmunkInitialized = unsafePerformIO $ newIORef False
{-# NOINLINE _chipmunkInitialized #-}


-- | A `Space` is a mutable collection of bodies and constraints in
-- a physical simulation. All operations on `Space` are in IO.
--
-- This is essentially the Hipmunk type `H.Space`
-- with additional bookkeeping.
newtype Space = Space { hipmunkSpace :: H.Space }

instance Eq Space where
    (==) = (==) `on` hipmunkSpace

instance Ord Space where
    compare = compare `on` hipmunkSpace


-- | Create a new empty `Space`.
newSpace :: IO Space
newSpace = do
    b <- readIORef _chipmunkInitialized
    unless b $ do
        H.initChipmunk
        writeIORef _chipmunkInitialized True
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
