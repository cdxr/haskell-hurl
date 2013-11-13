module Physics.Hurl.Object
(
    -- * Objects
    Object,
    Object',
    SolidRef,

    -- ** Inspection
    viewSolids,
    viewShapes,
    absoluteShapes,

    -- ** Mutable Properties
    position,

    -- * DynamicObjects
    DynamicObject,
    dynamicObject,

    -- ** Applying Forces
    Force,
    applyForce,
    applyOnlyForce,
    applyImpulse,

    -- ** Mutable properties
    -- *** Velocity
    velocity,
    angVel,
    -- *** Force
    force,
    torque,
    -- *** Constraints
    maxVelocity,
    maxAngVel,
)
where

import Linear.V2

import Control.Lens
import Control.Applicative

import Data.StateVar ( StateVar, makeStateVar, get, ($=) )

import qualified Physics.Hipmunk as H

import Physics.Hurl.Geometry
import Physics.Hurl.Solid

import Physics.Hurl.Internal.Space
import Physics.Hurl.Internal.Utils


type Object' = Object []


-- | Output the `Solid`s contained in the `Object`.
viewSolids :: (Functor f) => Object f -> IO (f Solid)
viewSolids o = do
    p  <- get $ position o
    return $ (shape %~ moveShape p) . solidRefProto <$> objectSolids o


-- | Output the `Shape`s contained in the `Object`.
viewShapes :: (Functor f) => Object f -> IO (f Shape)
viewShapes = (fmap.fmap) (view shape) . viewSolids


-- | Output the `Shape`s contained in the `Object`, but with each shape
-- translated into absolute space coordinates.
absoluteShapes :: (Functor f) => Object f -> IO (f Shape)
absoluteShapes o = do
    p <- get $ position o
    fmap (moveShape p) <$> viewShapes o


position :: Object f -> StateVar Position
position o = makeStateVar g s
  where
    g = Pos <$> get (posVar o) <*> get (angleVar o)
    s (Pos p a) = do
        posVar   o $= p
        angleVar o $= a

    posVar :: Object f -> StateVar (V2 Double)
    posVar = varVectorToV2 . H.position . objectBody

    angleVar :: Object f -> StateVar Double
    angleVar = H.angle . objectBody


velocity :: DynamicObject f -> StateVar (V2 Double)
velocity = varVectorToV2 . H.velocity . dynamicBody

angVel :: DynamicObject f -> StateVar Double
angVel = H.angVel . dynamicBody


maxVelocity :: DynamicObject f -> StateVar Double
maxVelocity = H.maxVelocity . dynamicBody

maxAngVel :: DynamicObject f -> StateVar Double
maxAngVel = H.maxAngVel . dynamicBody


type Force = Double

force :: DynamicObject f -> StateVar (V2 Force)
force = varVectorToV2 . H.force . dynamicBody

torque :: DynamicObject f -> StateVar Force
torque = H.torque . dynamicBody


applyForce :: V2 Double -> V2 Force -> DynamicObject f -> IO ()
applyForce p v o = H.applyForce (dynamicBody o) (vectorFromV2 p) (vectorFromV2 v)

applyOnlyForce :: V2 Double -> V2 Force -> DynamicObject f -> IO ()
applyOnlyForce p v o = H.applyOnlyForce (dynamicBody o) (vectorFromV2 p) (vectorFromV2 v)

applyImpulse :: V2 Double -> V2 Force -> DynamicObject f -> IO ()
applyImpulse p v o = H.applyImpulse (dynamicBody o) (vectorFromV2 p) (vectorFromV2 v)
