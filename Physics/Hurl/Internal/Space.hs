module Physics.Hurl.Internal.Space where


import Control.Applicative

import Data.Function ( on )

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.Resource


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
newSpace = Space <$> H.newSpace


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
