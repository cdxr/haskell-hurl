module Physics.Hurl.Constraint where

import Linear.V2
import Control.Applicative

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.Space


newtype Constraint = Constraint (IO ())


-- | @pinObjects p a b@ creates a `Pin` between objects @a@ and @b@ at
-- global position @p@.
pinObjects :: V2 Double -> ObjectRef f -> ObjectRef g -> IO Constraint
pinObjects (V2 x y) ao bo = do
    c <- H.newConstraint a b =<< makeHipmunkPin
    H.spaceAdd space c
    return $ Constraint $ H.spaceRemove space c
  where
    pos   = H.Vector x y
    a     = objectBody ao
    b     = objectBody bo
    makeHipmunkPin = H.Pin <$> H.worldToLocal a pos <*> H.worldToLocal b pos
    space = hipmunkSpace . objectSpace $ ao


type BiasCoef = Double
type MaxForce = Double

pivotObjects :: BiasCoef -> MaxForce -> V2 Double -> ObjectRef f -> ObjectRef g -> IO Constraint
pivotObjects bc mf (V2 x y) ao bo = do
    c <- H.newConstraint a b $ H.Pivot1 pos
    H.setBiasCoefC bc c
    H.setMaxForce mf c
    H.spaceAdd space c
    return $ Constraint $ H.spaceRemove space c
  where
    pos   = H.Vector x y
    a     = objectBody ao
    b     = objectBody bo
    space = hipmunkSpace . objectSpace $ ao



deleteConstraint :: Constraint -> IO ()
deleteConstraint (Constraint r) = r
