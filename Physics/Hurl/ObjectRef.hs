module Physics.Hurl.ObjectRef
(
   ObjectRef
 , SolidRef

-- * Applying Forces
 , Force
 , applyForce
 , applyOnlyForce
 , applyImpulse

-- * Mutable properties
-- ** Position
 , position
 , angle
-- ** Velocity
 , velocity
 , angVel
 , maxVelocity
 , maxAngVel
-- ** Force
 , force
 , torque
)
where

import Linear.V2

import Data.StateVar ( StateVar )

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.ObjectRef
import Physics.Hurl.Internal.Utils


position :: ObjectRef f -> StateVar (V2 Double)
position = varVectorToV2 . H.position . objectBody

angle :: ObjectRef f -> StateVar Double
angle = H.angle . objectBody


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
