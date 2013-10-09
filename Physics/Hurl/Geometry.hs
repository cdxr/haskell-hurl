module Physics.Hurl.Geometry where

import Linear
import Control.Lens ( over )
import Control.Applicative

import Data.Monoid
import Data.Traversable ( traverse )
import Data.List ( foldl1' )


-- TODO distinguish between vectors and points


type Point = V2 Double


data Shape
    = Circle Double Point         -- ^ A circle with radius and position
    | Segment Double Point Point  -- ^ A line segment with thickness and two positions
    | Poly [Point]                -- ^ A convex polygon with clockwise winding
    deriving (Show, Eq, Ord)


points :: (Applicative f) => (Point -> f Point) -> Shape -> f Shape
points f s = case s of
    Circle r p     -> Circle r <$> f p
    Segment th p q -> Segment th <$> f p <*> f q
    Poly ps        -> Poly <$> traverse f ps


-- | @rectangle w h@ is a centered rectangle with width @w@ and height @h@.
rectangle :: Double -> Double -> Shape
rectangle w h = Poly [V2 (-x) (-y), V2 (-x) y, V2 x y, V2 x (-y)]
      where x = w/2
            y = h/2


translate :: Position -> Shape -> Shape
translate v = over points (translatePoint v)


translatePoint :: Position -> Point -> Point
translatePoint v (V2 x y) =
    let V4 x' y' 0 0 = posMatrix v !* V4 x y 0 0
    in V2 x' y'


------------------------------------------------------------------------------
-- TODO improve the efficiency of these operations
------------------------------------------------------------------------------


-- | A 2D translation and rotation.
data Position = Pos
    { trans :: !Point
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
