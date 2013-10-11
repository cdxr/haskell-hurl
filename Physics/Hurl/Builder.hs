module Physics.Hurl.Builder where

import Data.Foldable ( Foldable, foldMap )

import Physics.Hurl.Object
import Physics.Hurl.Solid
import Physics.Hurl.Geometry


-- | A `Solid` with material density.
data Dense = Dense Double Solid
    deriving (Show, Eq, Ord)

manifest :: (Foldable f) => f Dense -> Object f
manifest ds = 
  where
    (Sum mass, Sum moment) = foldMap massAndMoment ds
