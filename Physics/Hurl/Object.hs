{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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
    ) where

import Data.Functor.Identity

import Physics.Hurl.Solid


-- Note: we use FlexibleInstances and UndecidableInstances to implement
-- Show and Ord instances for `Object f` when `f Solid` implements them.


type Mass = Double
type Moment = Double

-- | A dynamic body with a mass and moment of inertia.
data Body = Body Mass Moment
    deriving (Show, Read, Eq, Ord)


-- | An @Object f@ is a @Body@ with a traversable @f@ of attached @Solid@s.
data Object f = Object
    { body   :: Body
    , solids :: f Solid
    }

deriving instance (Show (f Solid)) => Show (Object f)
deriving instance (Eq (f Solid))   => Eq (Object f)
deriving instance (Ord (f Solid))  => Ord (Object f)


type Object' = Object Identity


-- | @simpleObject m s@ is the an `Object'` of mass @m@ containing the
-- single solid @s@, with a moment of inertia calculated from @m@ and @s@.
simpleObject :: Mass -> Solid -> Object'
simpleObject m s = Object (Body m moment) $ Identity s
  where
    moment = momentForSolid s m 0
