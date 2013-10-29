module Physics.Hurl.Internal.Shape where

import Linear.V2

import qualified Physics.Hipmunk as H

import Physics.Hurl.Geometry
import Physics.Hurl.Internal.Utils


hipmunkPolygon :: [V2 Double] -> H.ShapeType
hipmunkPolygon = H.Polygon . map vectorFromV2


-- | Create a Hipmunk `H.Shape` from a Hurl `Shape` and a Hipmunk `H.Body`.
makeHipmunkShape :: Shape -> H.Body -> IO H.Shape
makeHipmunkShape s b = H.newShape b shapeType position
  where
    position = case s of
        Circle _ pos -> vectorFromV2 pos
        _            -> H.Vector 0 0
    shapeType = case s of
        Circle radius _ -> H.Circle radius
        Segment th p q  -> H.LineSegment (vectorFromV2 p) (vectorFromV2 q) th
        Poly ps         -> hipmunkPolygon ps

