{-# LANGUAGE DataKinds #-}

module Physics.Hurl.ObjectRef
(
    ObjectRef,
    ObjectRef',
    SolidRef,

    -- * Inspection
    viewSolids,
    viewShapes,

    -- * Applying Forces
    Force,
    applyForce,
    applyOnlyForce,
    applyImpulse,

    -- * Mutable properties
    -- ** Position
    position,
    -- ** Velocity
    velocity,
    angVel,
    -- ** Force
    force,
    torque,
    -- ** Constraints
    maxVelocity,
    maxAngVel,
)
where

import Linear.V2

import Control.Lens
import Control.Applicative

import Data.Functor.Identity

import Data.StateVar ( StateVar, makeStateVar, get, ($=) )

import qualified Physics.Hipmunk as H

import Physics.Hurl.Geometry
import Physics.Hurl.Solid

import Physics.Hurl.Internal.Space
import Physics.Hurl.Internal.Utils


type ObjectRef' = ObjectRef Identity


-- | Output each `Solid` in the ObjectRef.
viewSolids :: (Functor f) => ObjectRef f t -> IO (f Solid)
viewSolids o = do
    p  <- get $ position o
    return $ (shape %~ moveShape p) . solidRefProto <$> objectSolids o


-- | Output each `Shape` in the ObjectRef.
viewShapes :: (Functor f) => ObjectRef f t -> IO (f Shape)
viewShapes = (fmap.fmap) (view shape) . viewSolids


position :: ObjectRef f t -> StateVar Position
position o = makeStateVar g s
  where
    g = Pos <$> get (posVar o) <*> get (angleVar o)
    s (Pos p a) = do
        posVar   o $= p
        angleVar o $= a

    posVar :: ObjectRef f t -> StateVar (V2 Double)
    posVar = varVectorToV2 . H.position . objectBody

    angleVar :: ObjectRef f t -> StateVar Double
    angleVar = H.angle . objectBody


velocity :: ObjectRef f Dynamic -> StateVar (V2 Double)
velocity = varVectorToV2 . H.velocity . objectBody

angVel :: ObjectRef f Dynamic -> StateVar Double
angVel = H.angVel . objectBody


maxVelocity :: ObjectRef f Dynamic -> StateVar Double
maxVelocity = H.maxVelocity . objectBody

maxAngVel :: ObjectRef f Dynamic -> StateVar Double
maxAngVel = H.maxAngVel . objectBody


type Force = Double

force :: ObjectRef f Dynamic -> StateVar (V2 Force)
force = varVectorToV2 . H.force . objectBody

torque :: ObjectRef f Dynamic -> StateVar Force
torque = H.torque . objectBody


applyForce :: V2 Double -> V2 Force -> ObjectRef f Dynamic -> IO ()
applyForce p v o = H.applyForce (objectBody o) (vectorFromV2 p) (vectorFromV2 v)

applyOnlyForce :: V2 Double -> V2 Force -> ObjectRef f Dynamic -> IO ()
applyOnlyForce p v o = H.applyOnlyForce (objectBody o) (vectorFromV2 p) (vectorFromV2 v)

applyImpulse :: V2 Double -> V2 Force -> ObjectRef f Dynamic -> IO ()
applyImpulse p v o = H.applyImpulse (objectBody o) (vectorFromV2 p) (vectorFromV2 v)
