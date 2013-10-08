module Physics.Hurl.Query where

import Control.Monad
import Linear.V2

import Data.Foldable ( Foldable, toList )

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.ObjectRef


-- | @querySolid p s@ determines if the solid @s@ contains the point @p@.
querySolid :: V2 Double -> SolidRef -> IO Bool
querySolid (V2 x y) s = H.shapePointQuery s (H.Vector x y)

-- | @queryObject p o@ computes a list of all solids in @o@ that contain the 
-- point @p@.
queryObject :: (Foldable f) => V2 Double -> ObjectRef f -> IO [SolidRef]
queryObject p = filterM (querySolid p) . toList . objectSolids
