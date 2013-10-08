module Physics.Hurl.Solid
(
    -- * Solid
    Solid (..),
    solid,
    momentForSolid,
    -- * Mutable Properties
    elasticityRef,
    frictionRef,
    ) where


import Linear.V2
import Data.StateVar as StateVar

import qualified Physics.Hipmunk as H

import Physics.Hurl.Shape ( Shape )

import Physics.Hurl.Internal.ObjectRef


-- TODO
--  move elasticityRef and frictionRef to a SolidRef module
--  ? create a `Material` type

-- | A collision-capable entity consisting of geometry and material
-- properties.
data Solid = Solid
    { elasticity :: !Double
    , friction   :: !Double
    , shape      :: !Shape
    } deriving (Show, Eq, Ord)


-- | @solid s@ is the `Solid` made of the shape @s@ with zero elasticity
-- and friction.
solid :: Shape -> Solid
solid = Solid 0 0


-- | @momentForSolid s m o@ calculates the moment of intertia for the solid 
-- @s@ of mass @m@ at offset @o@.
momentForSolid :: Solid -> Double -> V2 Double -> Double
momentForSolid s m (V2 x y) = H.momentForShape m (shape s) (H.Vector x y)



-- | The elasticity of the solid in the range 0 to 1.
elasticityRef :: SolidRef -> StateVar Double
elasticityRef = H.elasticity


-- | The friction coefficient of the shape according to the Coulumb
-- friction model. The amount of applied friction is the product of the
-- friction of both solids.
frictionRef :: SolidRef -> StateVar Double
frictionRef = H.friction
