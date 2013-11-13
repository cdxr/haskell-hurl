{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- properties.hs
--
-- quickcheck properties for `hurl`
------------------------------------------------------------------------------

module Main ( main ) where

import Test.QuickCheck
import Test.QuickCheck.Shrink

import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Control.Applicative
import Data.Monoid
import Data.Function
import Data.Functor.Compose

import qualified Data.Foldable as F

import Data.AEq
import Data.Fixed

import Control.Lens hiding ( transform, elements )
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
    shrink = runShrink $ V2 <$> shrinks (view _x) <*> shrinks (view _y)

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

    shrink s = runShrink m s
      where
        m = case s of
            Circle r p -> Circle <$> shrinkingPos r <*> shrink' p
            Segment l a b ->
                Segment <$> shrinkingPos l <*> shrink' a <*> shrink' b
            s -> pure s
        shrinkingPos :: (Num a, Ord a, Arbitrary a) => a -> Shrink r a
        shrinkingPos = shrinksWith (filter (> 0) . shrink) . const
        --shrinkingPos = wrapping Positive shrink'

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


-- | Approx two angles for equality, measured in radians
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

prop_iso_translation :: V -> Property
prop_iso_translation v =
    v ==? isoTranslation (translate v)

prop_iso_rotation :: Double -> Property
prop_iso_rotation t =
    t `angleEq` isoRotation (rotate t)

prop_iso_full_rotate :: Double -> Property
prop_iso_full_rotate t =
    mempty ~==? rotate t <> rotate (2*pi - t)


prop_iso_reflect_inverse :: Property
prop_iso_reflect_inverse = once $
    isIsoId $ reflectY <> reflectY

prop_iso_reflectY :: Property
prop_iso_reflectY = once $ conjoin
    [ True  ==? isoReflectY (reflectY :: IsoD)
    , False ==? isoReflectY (mempty :: IsoD)
    ]
    


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

instance CoArbitrary Mass where
    coarbitrary = coarbitraryReal . getMass

instance AEq Mass where
    Mass a ~== Mass b = a ~== b

instance Arbitrary Moment where
    arbitrary = Moment <$> arbPositive
    shrink    = map Moment . shrinkRealFrac . getMoment

instance Arbitrary Density where
    arbitrary = Density <$> arbPositive
    shrink    = map Density . shrinkRealFrac . getDensity

instance CoArbitrary Density where
    coarbitrary = coarbitraryReal . getDensity

instance Arbitrary Surface where
    arbitrary = Surface <$> choose (0, 1) <*> choose (0, 1)
    shrink = runShrink $
        Surface <$> shrinks _friction <*> shrinks _elasticity

instance AEq Solid where
    a ~== b = and
        [ a^.density ~== b^.density
        , a^.surface ~== b^.surface
        , a^.shape   ~== b^.shape
        ]

instance AEq Density where
    Density a ~== Density b = a ~== b

instance AEq Surface where
    Surface f e ~== Surface f' e' = f ~== f' && e ~== e'

instance AEq Shape where
    Circle r p ~== Circle r' p' =
        and [r ~== r', p ~== p']
    Segment l a b ~== Segment l' a' b' =
        and [l ~== l', a ~== a', b ~== b']
    Poly ps ~== Poly qs =
        and $ zipWith (~==) ps qs
    _ ~== _ = False


instance Arbitrary Solid where
    arbitrary = makeSolid <$> arbitrary <*> arbitrary <*> arbitrary
    shrink = runShrink $ makeSolid
        <$> shrinks (view density)
        <*> shrinks (view surface)
        <*> shrinks (view shape)

-- | Verify the internal consistency of the `Solid`.
validSolid :: Solid -> Bool
validSolid (Solid ma d v mo surf sh) = and
    [ v  == Volume (area sh)
    , ma == mass' d v
    , mo == moment' ma sh
    ]


data NumFun a = Plus a | Times a | Replace a
    deriving (Show, Functor)

instance (Arbitrary a) => Arbitrary (NumFun a) where
    arbitrary = oneof $ map (<$> arbitrary) [Plus, Times, Replace]

applyNumFun :: (Num a) => NumFun a -> a -> a
applyNumFun nf = case nf of
    Plus  x -> (+ x)
    Times x -> (* x)
    Replace x -> const x
    

type Approx a = (Arbitrary a, Show a, AEq a)


isNumSetter
    :: (Arbitrary a, CoArbitrary a, Show a, Num a, Approx s)
    => Setter' s a -> Property
isNumSetter l =
    setter_id l .&.
    num_setter_composition l .&.
    approx_setter_set_set l

isNumTraversal
    :: (Arbitrary a, CoArbitrary a, Show a, Num a, Approx s)
    => Traversal' s a -> Property
isNumTraversal l =
    isNumSetter l .&.
    traverse_pureMaybe l .&.
    traverse_pureList l .&.
    do as <- arbitrary
       bs <- arbitrary
       t <- arbitrary
       property $ traverse_compose l (\x -> as++[x]++bs)
                                     (\x -> if t then Just x else Nothing)

num_setter_composition
    :: (Num a, Arbitrary a, Approx s)
    => Setter' s a -> s -> NumFun a -> NumFun a -> Property
num_setter_composition l s nf ng =
    let f = applyNumFun nf
        g = applyNumFun ng
    in over l f (over l g s) ~==? over l (f . g) s


isNumLens
    :: (Show a, AEq a, Num a, Arbitrary a, CoArbitrary a, Approx s)
    => Lens' s a -> Property
isNumLens l =
    approx_lens_set_view l .&.
    approx_lens_view_set l .&.
    isNumTraversal l


prop_mass_lens :: Property
prop_mass_lens = isNumLens mass

prop_density_lens :: Property
prop_density_lens = isNumLens density

--prop_friction_lens
--prop_elasticity_lens = isLens

------------------------------------------------------------------------------
-- taken from the lens package: "tests/properties.hs"


-- The first setter law:
setter_id :: Eq s => Setter' s a -> s -> Bool
setter_id l s = over l id s == s

approx_lens_set_view :: AEq s => Lens' s a -> s -> Bool
approx_lens_set_view l s = set l (view l s) s ~== s

approx_lens_view_set :: AEq a => Lens' s a -> s -> a -> Bool
approx_lens_view_set l s a = view l (set l a s) ~== a

approx_setter_set_set :: AEq s => Setter' s a -> s -> a -> a -> Bool
approx_setter_set_set l s a b = set l b (set l a s) ~== set l b s

prism_yin :: Eq a => Prism' s a -> a -> Bool
prism_yin l a = preview l (review l a) == Just a

prism_yang :: Eq s => Prism' s a -> s -> Bool
prism_yang l s = maybe s (review l) (preview l s) == s

traverse_pure :: forall f s a. (Applicative f, Eq (f s)) => LensLike' f s a -> s -> Bool
traverse_pure l s = l pure s == (pure s :: f s)

traverse_pureMaybe :: Eq s => LensLike' Maybe s a -> s -> Bool
traverse_pureMaybe = traverse_pure

traverse_pureList :: Eq s => LensLike' [] s a -> s -> Bool
traverse_pureList = traverse_pure

traverse_compose :: (Applicative f, Applicative g, Eq (f (g s)))
                    => Traversal' s a -> (a -> g a) -> (a -> f a) -> s -> Bool
traverse_compose t f g s = (fmap (t f) . t g) s == (getCompose . t (Compose . fmap f . g)) s

