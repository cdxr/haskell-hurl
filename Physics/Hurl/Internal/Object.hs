module Physics.Hurl.Internal.Object where

import Linear

import Data.Functor.Identity

import Physics.Hurl.Solid

import Physics.Hurl.Internal.ObjectRef


type Mass = Double
type Moment = Double

-- | A dynamic body with a mass and moment of inertia, or a motionless
-- static body.
data Body = Body Mass Moment | Static
    deriving (Show, Read, Eq, Ord)


data Object f = Object
    { initObject :: ObjectRef f -> IO ()
    , body      :: Body
    , solids    :: f (V2 Double, Solid)
    }


type Object'    = Object Identity
type ObjectRef' = ObjectRef Identity
