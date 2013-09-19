module Physics.Hurl.ObjectRef
(
   ObjectRef
 , createObjectRef
 , deleteObjectRef

-- * Applying Forces
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

import Data.Foldable ( Foldable, foldMap )
import Data.Monoid

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.Resource
import Physics.Hurl.Internal.Space
import Physics.Hurl.Internal.Utils


data ShapeMode = Static | Dynamic
    deriving (Show, Read, Eq, Ord)


-- | A reference to a physical entity in a `Space`.
data ObjectRef f = ObjectRef
    { objectSpace     :: Space
    , objectBody      :: H.Body
    , objectShapes    :: f H.Shape
    , objectFinalizer :: IO ()
    }


-- | Create an `ObjectRef` in the `Space`.
createObjectRef :: (Foldable f) => ShapeMode -> H.Body -> f H.Shape -> Space -> IO (ObjectRef f)
createObjectRef shapeMode b ss space = do
    let resource = spaceResource b <> foldMap mkShape ss

    fin <- runResource resource space

    return $ ObjectRef space b ss fin
  where
    mkShape = case shapeMode of
        Static  -> spaceResource . H.Static
        Dynamic -> spaceResource


-- | Delete an `ObjectRef`.
deleteObjectRef :: ObjectRef f -> IO ()
deleteObjectRef = objectFinalizer



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
