module Physics.Hurl.Shape
(
  H.ShapeType
, Thickness
, lineSegment
, circle
, polygon
, rectangle
)
where

import Linear.V2
import Linear.Affine

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.Utils


type Thickness = Double

lineSegment :: Thickness -> V2 Double -> V2 Double -> H.ShapeType
lineSegment t a b = H.LineSegment (vectorFromV2 a) (vectorFromV2 b) t

circle :: Double -> H.ShapeType
circle = H.Circle

polygon :: [V2 Double] -> H.ShapeType
polygon = H.Polygon . map vectorFromV2

rectangle :: Double -> Double -> H.ShapeType
rectangle w h = H.Polygon $ map (uncurry H.Vector)
                    [(-x, -y), (-x, y), (x, y), (x, -y)]
      where x = w/2
            y = h/2


