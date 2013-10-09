module Physics.Hurl.Position where

import Linear

import Data.Monoid
import Data.List ( foldl1' )


------------------------------------------------------------------------------
-- TODO improve the efficiency of all these operations
------------------------------------------------------------------------------


-- | A 2D position within the simulation space, consisting of a translation
-- and rotation.
data Position = Pos
    { trans :: !(V2 Double)
    , angle :: !Double
    } deriving (Show, Eq, Ord)


instance Monoid Position where
    mempty = Pos 0 0
    a `mappend` b = unsafeFromMatrix $ posMatrix a !*! posMatrix b
    mconcat ps = case ps of
        []  -> mempty
        [p] -> p
        _   -> unsafeFromMatrix $ foldl1' (!*!) . map posMatrix $ ps


unsafeFromMatrix :: M44 Double -> Position
unsafeFromMatrix (V4 (V4 _ _ _ x) (V4 c d _ y) _ _) = Pos (V2 x y) (atan (c/d))

posMatrix :: Position -> M44 Double
posMatrix (Pos (V2 x y) r) = mkTransformation quat (V3 x y 0)
  where
    quat = axisAngle (V3 0 0 1) r
