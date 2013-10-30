{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- properties.hs
--
-- quickcheck properties for `hurl`
------------------------------------------------------------------------------

module Main where

import Test.QuickCheck
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Control.Applicative
import Data.Monoid
import Data.Function

import qualified Data.Foldable as F
import Data.Traversable ( traverse )

import Data.AEq
import Data.Fixed

import Control.Lens  ( view )
import Linear hiding ( rotate )

import Physics.Hurl

import Physics.Hurl.Geometry.Isometry
--import Physics.Hurl.Geometry
import Physics.Hurl.Internal.Solid


main = $defaultMainGenerator



arbPositive :: (Num a, Ord a, Arbitrary a) => Gen a
arbPositive = getPositive <$> arbitrary 


------------------------------------------------------------------------------
-- typeclass instances and generators
------------------------------------------------------------------------------

instance (Arbitrary a) => Arbitrary (V2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary
    shrink (V2 x y) = tail $ V2 <$> (x : shrink x) <*> (y : shrink y)

instance (AEq a) => AEq (V2 a) where
    a ~== b = F.and $ (~==) <$> a <*> b

instance (AEq a) => AEq (V3 a) where
    a ~== b = F.and $ (~==) <$> a <*> b


instance Arbitrary (Isometry Double) where
    arbitrary = mconcat <$> sequence
        [ translate <$> arbitrary
        , rotate    <$> arbitrary
        , elements [mempty, reflectY]
        ]
    shrink = map unsafeFromMatrix
           . (traverse.traverse) shrinkRealFrac
           . isometryMatrix

instance AEq (Isometry Double) where
    i ~== j = and
        [ on (==)  isoReflectY i j
        , on (~==) (cos . isoRotation) i j
        , on (~==) isoTranslation i j
        ]


instance Arbitrary Shape where
    arbitrary = oneof
        [ Circle  <$> arbPositive <*> arbitrary
        , do (a, b) <- distinct arbitrary
             Segment <$> arbPositive <*> pure a <*> pure b
        -- , Poly  -- TODO
        ]
      where
        distinct :: (Eq a) => Gen a -> Gen (a, a)
        distinct g = do
            a <- g
            (,) a <$> g `suchThat` (/= a)

-- test the `Shape` Arbitrary instance
prop_valid :: Shape -> Bool
prop_valid = validShape




------------------------------------------------------------------------------
-- QuickCheck Utilities
------------------------------------------------------------------------------

(==?) :: (AEq a, Show a) => a -> a -> Property
a ==? b = printTestCase s $ a == b
  where
    s = unwords [show a, "==?", show b]

(~==?) :: (AEq a, Show a) => a -> a -> Property
a ~==? b = printTestCase s $ a ~== b
  where
    s = unwords [show a, "~==?", show b]

infix 4 ==?
infix 4 ~==?


-- | Compare two angles for equality, measured in radians
angleEq :: Double -> Double -> Property
angleEq t u = mod' t tau ~==? mod' u tau
  where
    tau = pi * 2

------------------------------------------------------------------------------
-- Physics.Hurl.Geometry.Isometry
------------------------------------------------------------------------------

comparingFun :: (Show a, Arbitrary a)
    => (b -> b -> Bool) -> (a -> b) -> (a -> b) -> Property
comparingFun comp f g = forAll arbitrary $ comp <$> f <*> g


type IsoD = Isometry Double
type V    = V2 Double

--eqIsometry :: IsoD -> IsoD -> Property
--eqIsometry a b = forAll arbitrary $ eqV2 <$> transform a <*> transform b


prop_iso_matrix :: IsoD -> Property
prop_iso_matrix i = i ~==? (unsafeFromMatrix . isometryMatrix) i


prop_iso_assoc :: IsoD -> IsoD -> IsoD -> Property
prop_iso_assoc a b c = 
    classify (any isoReflectY [a,b,c]) "reflected" $
        (a <> b) <> c ~==? a <> (b <> c)

prop_iso_distrib :: IsoD -> IsoD -> V2 Double -> Property
prop_iso_distrib a b v =
    (transform a . transform b) v ~==? transform (a <> b) v


prop_iso_leftidentity :: IsoD -> Property
prop_iso_leftidentity i =
    i ==? mempty <> i

prop_iso_rightidentity :: IsoD -> Property
prop_iso_rightidentity i =
    i ==? i <> mempty

prop_iso_identity :: V -> Property
prop_iso_identity v =
    v ~==? transform mempty v

isIsoId :: IsoD -> Property
isIsoId = (~==? mempty)

prop_iso_translate_inverse :: V -> Property
prop_iso_translate_inverse v =
    isIsoId $ translate v <> translate (negate v)

prop_iso_rotate_inverse :: Double -> Property
prop_iso_rotate_inverse t =
    isIsoId $ rotate t <> rotate (negate t)

prop_iso_reflect_inverse :: Property
prop_iso_reflect_inverse =
    isIsoId $ reflectY <> reflectY


prop_iso_translation :: V -> Property
prop_iso_translation v =
    v ==? isoTranslation (translate v)

prop_iso_rotation :: Double -> Property
prop_iso_rotation t =
    t `angleEq` isoRotation (rotate t)

prop_iso_full_rotate :: Double -> Property
prop_iso_full_rotate t =
    mempty ~==? rotate t <> rotate (2*pi - t)

prop_iso_reflectY :: Bool -> Property
prop_iso_reflectY b =
    b ==? isoReflectY (if b then reflectY else mempty)


------------------------------------------------------------------------------
-- Physics.Hurl.Geometry
------------------------------------------------------------------------------

-- | @shapePoint s@ generates a point contained by the shape @s@
shapePoint :: Shape -> Gen (V2 Double)
shapePoint s = undefined  -- TODO


prop_transformShape :: Isometry Double -> Shape -> Property
prop_transformShape i s0 = conjoin
    [ printTestCase "valid"          $ validShape s
    , let (a0, a) = (area s0, area s)
      in printTestCase
            (unwords [ "area s0 =", show a0
                     , "~=="
                     , show a, "= area s"
                     ])
            $ a0 ~== a
    -- TODO: forAll (shapePoints s) $ \p -> containsPoint p s
    ]
  where s = transformShape i s0


------------------------------------------------------------------------------
-- Physics.Hurl.Solid
------------------------------------------------------------------------------

instance Arbitrary Mass where
    arbitrary = Mass <$> arbPositive
    shrink    = map Mass . shrinkRealFrac . getMass

instance Arbitrary Moment where
    arbitrary = Moment <$> arbPositive
    shrink    = map Moment . shrinkRealFrac . getMoment

instance Arbitrary Density where
    arbitrary = Density <$> arbPositive
    shrink    = map Density . shrinkRealFrac . getDensity

instance Arbitrary Surface where
    arbitrary = Surface <$> choose (0, 1) <*> choose (0, 1)

instance Arbitrary Solid where
    arbitrary = oneof
        [ solid <$> arbitrary
        , makeSolid <$> arbitrary <*> arbitrary <*> arbitrary
        , makeSolidMass <$> arbitrary <*> arbitrary <*> arbitrary
        ]
    shrink s = makeSolid
        <$> shrink (view density s)
        <*> shrink (view surface s)
        <*> shrink (view shape s)


-- | Verify the internal consistency of the `Solid`.
validSolid :: Solid -> Bool
validSolid (Solid ma d v mo surf sh) = and
    [ v  == Volume (area sh)
    , ma == mass' d v
    , mo == moment' ma sh
    ]
