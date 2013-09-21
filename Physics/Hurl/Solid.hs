{-# LANGUAGE TemplateHaskell #-}

module Physics.Hurl.Solid
(
   Solid
 , solid
 , solidWith
 , Shape
 , shape
 , addToBody
 , querySolid
 , momentForSolid
-- * Mutable Properties
 , elasticity
 , friction
)
where

import Linear.V2
import Control.Lens.TH
import Data.StateVar as StateVar

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.ObjectRef


-- TODO
--  move addToBody to an internal module
--  move elasticity and friction to a SolidRef module
--  create a Material type


type Shape = H.ShapeType


-- | An entity capable of collisions, attached to a `Body` and consisting of
-- geometry and material properties.
data Solid = Solid
    { initSolid :: SolidRef -> IO ()
    , _shape    :: Shape
    }

$(makeLenses ''Solid)


-- | Create a `Solid` with default properties from a `Shape`.
solid :: Shape -> Solid
solid = Solid (\_ -> return ())


-- | @momentForSolid s m o@ calculate the moment of intertia for the solid 
-- @s@ of mass @m@ at offset @o@.
momentForSolid :: Solid -> Double -> V2 Double -> Double
momentForSolid s m (V2 x y) = H.momentForShape m (_shape s) (H.Vector x y)


-- | Create a `Solid` from a shape and a `SolidRef` action.
solidWith :: Shape -> (SolidRef -> IO ()) -> Solid
solidWith = flip Solid



-- | @addToBody p s b@ adds the `Solid` @s@ to the Hipmunk `H.Body` at
-- position @p@.
addToBody :: V2 Double -> Solid -> H.Body -> IO SolidRef
addToBody (V2 x y) (Solid initSolid shape) b = do
    ref <- H.newShape b shape (H.Vector x y)
    initSolid ref
    return ref


-- | The elasticity of the solid in the range 0 to 1.
elasticity :: SolidRef -> StateVar Double
elasticity = H.elasticity


-- | The friction coefficient of the shape according to the Coulumb
-- friction model. The amount of applied friction is the product of the
-- friction of both solids.
friction :: SolidRef -> StateVar Double
friction = H.friction
