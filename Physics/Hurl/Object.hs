module Physics.Hurl.Object (
    -- * Bodies
    Mass,
    Moment,
    Body (..),
    -- * Objects
    Object,
    object,
    body,
    solids,
    -- * Simple Objects
    Object',
    ObjectRef',
    simpleObject,
    simpleStatic,
    ) where

import Linear

import Data.Functor.Identity

import Physics.Hurl.Solid

import Physics.Hurl.Internal.Object


object :: Body -> f (V2 Double, Solid) -> Object f
object = Object (\_ -> return ())

simpleObject :: Mass -> Solid -> Object'
simpleObject m s = object (Body m moment) $ Identity (0, s)
  where
    moment = momentForSolid s m 0

simpleStatic :: Solid -> Object'
simpleStatic s = object Static $ Identity (0, s)
