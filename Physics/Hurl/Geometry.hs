module Physics.Hurl.Geometry
(
    -- * Shape
    Shape (..),
    validShape,
    area,
    transformShape,
    translateShape,
    rectangle,

    -- * Position
    Position(..),
    positionIso,
    moveShape,

    module Physics.Hurl.Geometry.Isometry,

    -- * Linear Re-exports
    V2 (..),
) where

import Control.Applicative
import Data.Monoid
import Data.Traversable ( traverse )

import Linear hiding ( rotate )
import Control.Lens ( over )

import Physics.Hurl.Geometry.Isometry


data Shape
    = Circle Double (V2 Double)
        -- ^ A circle with positive radius and position
    | Segment Double (V2 Double) (V2 Double)
        -- ^ A line segment with positive thickness and two positions
    | Poly [V2 Double]
        -- ^ A convex polygon with clockwise winding
    deriving (Show, Eq, Ord)




validShape :: Shape -> Bool
validShape s = case s of
    Circle r _    -> r > 0
    Segment t a b -> t > 0 && a /= b
    Poly ps       -> length ps > 2  -- TODO verify clockwise and convex

area :: Shape -> Double
area s = case s of
    Circle r _    -> r * r
    Segment t a b -> t * distance a b
    Poly ps       -> polygonAreaCCW (reverse ps)


-- | @polygonArea ps@ is the area of the convex polygon given by the
-- counter-clockwise points @ps@.
polygonAreaCCW :: [V2 Double] -> Double
polygonAreaCCW ps = 0.5 * f xs ys - f ys xs
  where
    (xs, ys) = unzip $ map (\(V2 x y) -> (x,y)) ps
    f as bs  = sum $ zipWith (*) as $ drop 1 bs ++ bs


transformShape :: Isometry Double -> Shape -> Shape
transformShape i = over points $ transform i

translateShape :: V2 Double -> Shape -> Shape
translateShape = transformShape . translate

rotateShape :: Double -> Shape -> Shape
rotateShape = transformShape . rotate

reflectShapeY :: Shape -> Shape
reflectShapeY = transformShape reflectY


-- | @points :: Traversal' Shape Point@
points :: (Applicative f) => (V2 Double -> f (V2 Double)) -> Shape -> f Shape
points f s = case s of
    Circle r p     -> Circle r <$> f p
    Segment th p q -> Segment th <$> f p <*> f q
    Poly ps        -> Poly <$> traverse f ps


-- | @rectangle w h@ is a centered rectangle with width @w@ and height @h@.
rectangle :: Double -> Double -> Shape
rectangle w h = Poly [V2 (-x) (-y), V2 (-x) y, V2 x y, V2 x (-y)]
      where x = w/2
            y = h/2


{-
translate :: Position -> Shape -> Shape
translate v = over points (translatePoint v)


translatePoint :: Position -> Point -> Point
translatePoint v (V2 x y) =
    let V4 x' y' 0 0 = posMatrix v !* V4 x y 0 0
    in V2 x' y'
-}


------------------------------------------------------------------------------
-- TODO improve the efficiency of these operations
------------------------------------------------------------------------------


-- | A 2D translation and rotation.
data Position = Pos
    { trans :: !(V2 Double)
    , angle :: !Double
    } deriving (Show, Eq, Ord)


positionIso :: Position -> Isometry Double
positionIso (Pos t a) = translate t <> rotate a

moveShape :: Position -> Shape -> Shape
moveShape = transformShape . positionIso


{-
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
-}

