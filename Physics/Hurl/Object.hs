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
    simpleStatic,
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


simpleObject :: Mass -> Solid -> Object'
simpleObject m s = Object (Body m moment) (Identity (0, s))
  where
    moment = momentForSolid s m 0

simpleStatic :: Solid -> Object'
simpleStatic s = Object Static (Identity (0, s))
