module Physics.Hurl.Shape (
    Shape,
    Thickness,
    lineSegment,
    circle,
    polygon,
    rectangle,
    ) where

import Linear.V2

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.Utils


type Shape = H.ShapeType

type Thickness = Double

lineSegment :: Thickness -> V2 Double -> V2 Double -> Shape
lineSegment t a b = H.LineSegment (vectorFromV2 a) (vectorFromV2 b) t

circle :: Double -> Shape
circle = H.Circle

polygon :: [V2 Double] -> Shape
polygon = H.Polygon . map vectorFromV2

rectangle :: Double -> Double -> Shape
rectangle w h = H.Polygon $ map (uncurry H.Vector)
                    [(-x, -y), (-x, y), (x, y), (x, -y)]
      where x = w/2
            y = h/2
