{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Physics.Hurl.Internal.Solid where

import Linear

import Data.Monoid

import qualified Physics.Hipmunk as H

import Physics.Hurl.Geometry
import Physics.Hurl.Internal.Utils


-- TODO: consider adding a Moment multiplier to `Solid`


-- * Solid
--
-- Invariant: The mass and density of a Solid are mutually dependent;
-- changing one will recompute the other in respect to volume.
-- Changing the shape changes the volume, and recomputes the mass in respect
-- to density and volume.
--
-- Mass, Density, Surface, and Shape are "public" components of the Solid.
-- Volume and Moment are "private" cached components provided to enable sharing.

-- | A collision-capable entity consisting of geometry and material
-- properties.
--
data Solid = Solid
    { solidMass    :: Mass       -- ^ consistent with Density and Volume
    , solidDensity :: Density    -- ^ consistent with Mass and Volume
    , solidVolume  :: Volume     -- ^ cached, dependent on Shape
    , solidMoment  :: Moment     -- ^ cached, dependent on Mass and Volume
    , solidSurface :: !Surface   -- ^ surface properties
    , solidShape   :: !Shape     -- ^ 2d geometry
    } deriving (Eq, Ord)

instance Show Solid where
    showsPrec d s = showParen (d > 10)
        $ showString "makeSolid "
        . shows (getDensity $ solidDensity s)
        . showString " "
        . showParen True
            (shows (solidSurface s))
        . showString " "
        . showParen True
            (shows (solidShape s))

-- ** Constructors

-- | @solid s@ is a solid with shape @s@ and default values for all other
-- properties.
solid :: Shape -> Solid
solid sh = Solid ma d v mo s sh
  where
    ma = mass' d v
    d  = 1
    v  = Volume (area sh)
    mo = moment' ma sh
    s  = defaultSurface
{-# INLINABLE solid #-}

-- | @makeSolid d sf sh@ is the solid with density @d@, surface @sf@, and
-- shape @sh@.
makeSolid :: Density -> Surface -> Shape -> Solid
makeSolid d s = setDensity d . setSurface s . solid
{-# INLINABLE makeSolid #-}

-- | @makeSolidMass m sf sh@ is the solid with mass @m@, surface @sf@, and
-- shape @sh@.
makeSolidMass :: Mass -> Surface -> Shape -> Solid
makeSolidMass ma s = setMass ma . setSurface s . solid
{-# INLINABLE makeSolidMass #-}


-- * Transformations

-- | @setMass ma s@ is the solid with the shape and surface properties of @s@,
-- but with a mass of @ma@.
--
-- Note that a different Mass will also result in a different Density and
-- Moment.
setMass :: Mass -> Solid -> Solid
setMass ma s = unsafeUpdateMassDensity ma (density' ma v) s
  where
    v = solidVolume s
{-# INLINABLE setMass #-}


-- | @setDensity d s@ is the solid with the shape and surface properties of @s@,
-- but with a density of @d@.
--
-- Note that a different Density will also result in a different Mass and
-- Moment.
setDensity :: Density -> Solid -> Solid
setDensity d s = unsafeUpdateMassDensity (mass' d v) d s
  where
    v = solidVolume s
{-# INLINABLE setDensity #-}


-- | @setShape sh s@ is the solid with the surface properties and density
-- of @s@, but with the shape @sh@.
--
-- Note that a different shape will also result in a different Volume,
-- Mass, and Moment.
setShape :: Shape -> Solid -> Solid
setShape sh = updateVolume . unsafeIsometricShape sh
  where
    updateVolume s = s
        { solidVolume = v
        , solidMass   = ma
        }
        where
            ma = mass' (solidDensity s) v
            v  = Volume (area sh)
{-# INLINABLE setShape #-}


-- | @setSurface sf s@ is the solid with the physical properties of @s@,
-- but with surface properties @sf@.
setSurface :: Surface -> Solid -> Solid
setSurface surf s = s { solidSurface = surf }
{-# INLINABLE setSurface #-}



-- ** Unsafe

-- | Set the value of the Mass and Density simultaneously, updating all
-- dependent values.  The caller must ensure that the provided Mass and Density
-- are consistent with the Volume of the Solid.
unsafeUpdateMassDensity :: Mass -> Density -> Solid -> Solid
unsafeUpdateMassDensity ma d s = s
    { solidMass    = ma
    , solidDensity = d
    , solidMoment  = moment' ma $ solidShape s
    }
{-# INLINABLE unsafeUpdateMassDensity #-}


-- | Replace the Solid's shape with one that is known to have the same
-- area. This is more efficient than `setShape` because it
-- does not need to recalculate the Volume of the Solid (but it does
-- recalculate the Moment).
unsafeIsometricShape :: Shape -> Solid -> Solid
unsafeIsometricShape sh s = s
    { solidShape  = sh
    , solidMoment = moment' (solidMass s) sh
    }
{-# INLINABLE unsafeIsometricShape #-}


-- * Surface

-- | The material properties of a `Solid` that determine the surface behavior
-- of collisions
data Surface = Surface
    { _friction   :: !Friction
    , _elasticity :: !Elasticity
    } deriving (Show, Eq, Ord)

type Friction   = Double
type Elasticity = Double

-- | A `Surface` with `Friction`@=0.5@ and `Elasticity`@=0.5@.
defaultSurface :: Surface
defaultSurface = Surface 0.5 0.5


-- * Physical Properties

-- ** Types

newtype Mass = Mass { getMass :: Double }
    deriving (Num, Fractional, Show, Read, Eq, Ord)

instance Monoid Mass where
    mempty = Mass 0
    mappend (Mass a) (Mass b) = Mass (a + b)


newtype Moment = Moment { getMoment :: Double }
    deriving (Num, Fractional, Show, Read, Eq, Ord)

instance Monoid Moment where
    mempty = Moment 0
    mappend (Moment a) (Moment b) = Moment (a + b)


newtype Volume = Volume { getVolume :: Double }
    deriving (Num, Fractional, Show, Read, Eq, Ord)


newtype Density = Density { getDensity :: Double }
    deriving (Num, Fractional, Show, Read, Eq, Ord)


-- ** Equations

-- | @m = d * v@
mass' :: Density -> Volume -> Mass
mass' (Density d) (Volume v) = Mass (d * v)
{-# INLINABLE mass' #-}


-- | @d = m / v@
density' :: Mass -> Volume -> Density
density' (Mass m) (Volume v) = Density (m / v)
{-# INLINABLE density' #-}


-- | @v = m / d@
volume' :: Mass -> Density -> Volume
volume' (Mass m) (Density d) = Volume (m / d)
{-# INLINABLE volume' #-}


-- | @moment' m s@ is the mass moment of inertia for an object with shape
-- @s@ and uniformly-distributed mass @m@.
moment' :: Mass -> Shape -> Moment
moment' (Mass ma) sh = Moment $ case sh of
    Circle r off  -> ma * (quadrance off + r * r)
    Segment _ a b -> ma * (quadrance off + r * r / 12)
        where r = distance b a
              off = 0.5 *^ (a ^+^ b)
    Poly ps -> H.momentForPoly ma (toHipmunkPolygonOrder ps) 0
        where toHipmunkPolygonOrder = reverse . map vectorFromV2
{-# INLINABLE moment' #-}
