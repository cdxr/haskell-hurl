{-# LANGUAGE TemplateHaskell #-}

module Physics.Hurl.Solid
(
    -- * Solid
    Solid,
    solid,
    makeSolid,
    makeSolidMass,

    -- ** Lenses
    mass,
    density,
    shape,
    surface,

    -- ** Observable properties
    moment,
    volume,

    -- ** Surfaces
    Surface (..),
    friction,
    elasticity,

    -- ** Types
    Mass (..),
    Density (..),
    Moment (..),
    Volume (..),

    -- * Body
    Body(..),
    solidBody,

    ) where

import Control.Lens
import Linear

import Control.Applicative
import Data.Monoid

import Physics.Hurl.Geometry

import Physics.Hurl.Internal.Solid as I


$(makeLenses ''Surface)


-- | @volume s@ is the material volume of @s@, determined by the area of the
-- `Shape` of @s@.
volume :: Solid -> Volume
volume = solidVolume
{-# INLINABLE volume #-}


-- | @moment s@ is the mass moment of inertia of @s@, which is determined by 
-- the Mass and Shape of @s@.
moment :: Solid -> Moment
moment = solidMoment
{-# INLINABLE moment #-}


-- | A lens to the `Mass` of a Solid. Modifying the target of `mass`
-- also modifies the target of `density` relative to `volume`.
--
-- @
-- view mass s = mass' (view density s) (volume s)
-- @
--
mass :: Lens' Solid Mass
mass = lens solidMass $ flip setMass
{-# INLINABLE mass #-}

-- | A lens to the `Density` of a Solid. Modifying the target of `density`
-- also modifies the target of `mass` relative to `volume`.
--
-- @
-- view density s = density' (view mass s) (volume s)
-- @
--
density :: Lens' Solid Density
density = lens solidDensity $ flip setDensity
{-# INLINABLE density #-}

-- | A lens to the `Surface` of a Solid.
--
-- `surface` is orthogonal to every other property of a `Solid`.
surface :: Lens' Solid Surface
surface = lens solidSurface $ flip setSurface
{-# INLINABLE surface #-}

-- | A lens to the `Shape` of a `Solid`.
--
-- If modifying the target of `shape` results in a new value of `volume`,
-- the target of `mass` will also be modified relative to the value of
-- `density`.
shape :: Lens' Solid Shape
shape = lens solidShape $ flip setShape
{-# INLINABLE shape #-}



-- | @translate v s@ is the Solid @s@ translated by the vector @v@.
--
-- The following equality holds, but `translate` is more efficient:
--
-- @
-- translate v = over shape (translateShape v)
-- @
--
translate :: V2 Double -> Solid -> Solid
translate v s = unsafeIsometricShape (translateShape v $ view shape s) s



-- | A physical entity with mass and mass moment of inertia.
data Body = Body !Mass !Moment
    deriving (Show, Eq, Ord)

instance Monoid Body where
    mempty = Body mempty mempty
    mappend (Body ma mo) (Body ma' mo') = Body (ma <> ma') (mo <> mo')

-- | @solidBody s@ is the `Body` determined by the properties of @s@.
solidBody :: Solid -> Body
solidBody = Body <$> view mass <*> moment

