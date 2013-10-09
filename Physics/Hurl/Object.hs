module Physics.Hurl.Object (
    -- * Bodies
    Mass,
    Moment,
    Body (..),
    -- * Objects
    Object (..),
    -- ** Simple Objects
    Object',
    simpleObject,
    static
    ) where

import Linear

import Data.Functor.Identity

import Physics.Hurl.Solid


type Mass = Double
type Moment = Double

-- | A dynamic body with a mass and moment of inertia, or a motionless
-- static body.
data Body = Body Mass Moment | Static
    deriving (Show, Read, Eq, Ord)


-- | An @Object f@ is a @Body@ with a traversable @f@ of attached @Solid@s.
data Object f = Object
    { body   :: Body
    , solids :: f (V2 Double, Solid)
    }


type Object' = Object Identity


-- | @simpleObject m s@ is the an `Object'` of mass @m@ containing the
-- single solid @s@, with a moment of inertia calculated from @m@ and @s@.
simpleObject :: Mass -> Solid -> Object'
simpleObject m s = Object (Body m moment) (Identity (0, s))
  where
    moment = momentForSolid s m 0

-- | @static s@ is the static `Object'` containing the single solid @s@.
static :: Solid -> Object'
static s = Object Static (Identity (0, s))
