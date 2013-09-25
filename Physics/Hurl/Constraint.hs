module Physics.Hurl.Constraint where

import Linear.V2
import Control.Applicative

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.ObjectRef
import Physics.Hurl.Internal.Space


data Pin = Pin H.Space (H.Constraint H.Pin)
  deriving (Eq, Ord)


-- | @pinObjects p a b@ creates a `Pin` between objects @a@ and @b@ at
-- global position @p@.
pinObjects :: V2 Double -> ObjectRef f -> ObjectRef g -> IO Pin
pinObjects (V2 x y) ao bo = do
    c <- H.newConstraint a b =<< makeHipmunkPin
    H.spaceAdd space c
    return $ Pin space c
  where
    pos   = H.Vector x y
    a     = objectBody ao
    b     = objectBody bo
    makeHipmunkPin = H.Pin <$> H.worldToLocal a pos <*> H.worldToLocal b pos
    space = hipmunkSpace . objectSpace $ ao


deletePin :: Pin -> IO ()
deletePin (Pin space c) = H.spaceRemove space c
