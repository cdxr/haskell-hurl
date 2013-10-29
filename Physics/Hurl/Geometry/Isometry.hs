module Physics.Hurl.Geometry.Isometry
(
    -- * Isometry
    Isometry,
    isometryMatrix,

    -- ** Construction
    translate,
    rotate,
    reflectY,

    -- ** Transformations
    transform,
    transformV3,

    -- ** Viewing Components
    isoTranslation,
    isoRotation,
    isoReflectY,

    -- * Utilities
    unsafeFromMatrix,
    translationM33,
) where

import Control.Applicative
import Data.Monoid

import Control.Lens ( Lens' )
import Control.Lens.Getter
import Control.Lens.Setter
import Linear       hiding ( rotate )


-- | A lens to the translation vector of a 2d affine transformation matrix.
translationM33 :: (R3 v, R2 t) => Lens' (t (v a)) (V2 a)
translationM33 f rs = aux <$> f ((^._z) <$> rs^._xy)
  where
    aux (V2 x y) = (_x._z .~ x) . (_y._z .~ y) $ rs


-- | A Euclidian plane isometry. When a polygon is transformed isometrically,
-- its area does not change.
newtype Isometry a = Isometry { isometryMatrix :: M33 a }
    deriving (Eq, Ord)


isoTranslation :: Isometry a -> V2 a
isoTranslation = view translationM33 . isometryMatrix


isoRotation :: (RealFloat a) => Isometry a -> a
isoRotation (Isometry m) = atan2 cost (- nsint)
  where
    V3 cost nsint _ = m^._x


isoReflectY :: (Num a, Eq a) => Isometry a -> Bool
isoReflectY (Isometry m) = signum (m^._x._x) == signum (m^._y._y)


-- | Create an `Isometry` from a three-by-three matrix. This only creates
-- a valid `Isometry` if the given matrix is an isometric transformation,
-- i.e. preserves distances.
unsafeFromMatrix :: M33 a -> Isometry a
unsafeFromMatrix = Isometry


instance (Num a) => Monoid (Isometry a) where
    mempty = Isometry eye3
    mappend a b = Isometry (isometryMatrix b !*! isometryMatrix a)


translate :: (Num a) => V2 a -> Isometry a
translate (V2 x y) = Isometry $ V3
    (V3 1 0 x)
    (V3 0 1 y)
    (V3 0 0 1)

-- | Counter-clockwise rotation
rotate :: (Floating a) => a -> Isometry a
rotate t = Isometry $ V3
    (V3 c (-s) 0)
    (V3 s   c  0)
    (V3 0   0  1)
  where
    c = cos t
    s = sin t

reflectY :: (Num a) => Isometry a
reflectY = Isometry $ _y._y .~ negate 1 $ eye3


transform :: (Num a) => Isometry a -> V2 a -> V2 a
transform i (V2 x y) = transformV3 i (V3 x y 1) ^. _xy

transformV3 :: (Num a) => Isometry a -> V3 a -> V3 a
transformV3 i v = v *! isometryMatrix i
