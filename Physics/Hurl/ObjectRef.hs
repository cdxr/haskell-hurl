module Physics.Hurl.ObjectRef
(

    ObjectRef,
    ObjectRef',
    SolidRef,

    -- * Inspection
    getPosSolids,
    getPosShapes,

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

import Control.Applicative

import Data.Monoid
import Data.Functor.Identity

import Data.StateVar ( StateVar, makeStateVar, get, ($=) )

import qualified Physics.Hipmunk as H

import Physics.Hurl.Geometry
import Physics.Hurl.Solid

import Physics.Hurl.Internal.Space
import Physics.Hurl.Internal.Utils


type ObjectRef' = ObjectRef Identity


-- | Output each `Solid` in the ObjectRef.
getPosSolids :: (Functor f) => ObjectRef f -> IO (f Solid)
getPosSolids o = do
    p  <- get $ position o
    return $ move p . solidProto <$> objectSolids o
  where
    move :: Position -> Solid -> Solid
    move p s = s { shape = translate p (shape s) }


-- | Output each `Shape` in the ObjectRef.
getPosShapes :: (Functor f) => ObjectRef f -> IO (f Shape)
getPosShapes = (fmap.fmap) shape . getPosSolids


position :: ObjectRef f -> StateVar Position
position o = makeStateVar g s
  where
    g = Pos <$> get (posVar o) <*> get (angleVar o)
    s (Pos p a) = do
        posVar   o $= p
        angleVar o $= a

    posVar :: ObjectRef f -> StateVar (V2 Double)
    posVar = varVectorToV2 . H.position . objectBody

    angleVar :: ObjectRef f -> StateVar Double
    angleVar = H.angle . objectBody


velocity :: ObjectRef f -> StateVar (V2 Double)
velocity = varVectorToV2 . H.velocity . objectBody

angVel :: ObjectRef f -> StateVar Double
angVel = H.angVel . objectBody


maxVelocity :: ObjectRef f -> StateVar Double
maxVelocity = H.maxVelocity . objectBody

maxAngVel :: ObjectRef f -> StateVar Double
maxAngVel = H.maxAngVel . objectBody


type Force = Double

force :: ObjectRef f -> StateVar (V2 Force)
force = varVectorToV2 . H.force . objectBody

torque :: ObjectRef f -> StateVar Force
torque = H.torque . objectBody


applyForce :: V2 Double -> V2 Force -> ObjectRef f -> IO ()
applyForce p v o = H.applyForce (objectBody o) (vectorFromV2 p) (vectorFromV2 v)

applyOnlyForce :: V2 Double -> V2 Force -> ObjectRef f -> IO ()
applyOnlyForce p v o = H.applyOnlyForce (objectBody o) (vectorFromV2 p) (vectorFromV2 v)

applyImpulse :: V2 Double -> V2 Force -> ObjectRef f -> IO ()
applyImpulse p v o = H.applyImpulse (objectBody o) (vectorFromV2 p) (vectorFromV2 v)
