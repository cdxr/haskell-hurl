module Physics.Hurl.Constraint where

import Linear.V2

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.ObjectRef
import Physics.Hurl.Internal.Space


data Pin = Pin H.Space (H.Constraint H.Pin)
  deriving (Eq, Ord)


pinObjects :: (V2 Double, ObjectRef f) -> (V2 Double, ObjectRef g) -> IO Pin
pinObjects (V2 ax ay, ao) (V2 bx by, bo) = do
    c <- H.newConstraint (objectBody ao) (objectBody bo) hpin
    H.spaceAdd space c
    return $ Pin space c
  where
    hpin = H.Pin (H.Vector ax ay) (H.Vector bx by)
    space = hipmunkSpace . objectSpace $ ao


deletePin :: Pin -> IO ()
deletePin (Pin space c) = H.spaceRemove space c
