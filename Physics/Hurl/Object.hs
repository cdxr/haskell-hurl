{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Physics.Hurl.Object where

import Linear
import Control.Lens

import qualified Data.Traversable as Traversable

import Data.Functor.Identity

import Data.StateVar ( ($=) )

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.ObjectRef
import Physics.Hurl.Internal.Space


-- NOTE
-- We do not call or export an equivalent of H.initChipmunk here because it is
-- a no-op. See cpInitChipmunk in chipmunk.c


type Mass = Double
type Moment = Double

-- | A dynamic body with a mass and moment of inertia, or a motionless
-- static body.
data Body = Body Mass Moment | Static
    deriving (Show, Read, Eq, Ord)


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


data Object f = Object
    { initObject :: ObjectRef f -> IO ()
    , _body      :: Body
    , _solids    :: f (V2 Double, Solid)
    }

$(makeLenses ''Object)


type Object'    = Object Identity
type ObjectRef' = ObjectRef Identity

object :: Body -> f (V2 Double, Solid) -> Object f
object = Object (\_ -> return ())

simple :: Body -> Solid -> Object'
simple b = object b . Identity . (,) 0


create :: (Traversable f) => V2 Double -> Object f -> Space -> IO (ObjectRef f)
create (V2 x y) (Object init body solids) space = do
    b <- case body of
        Static     -> H.newBody H.infinity H.infinity
        Body ma mo -> H.newBody ma mo
    H.position b $= H.Vector x y

    shapes <- Traversable.forM solids $
        \(V2 x y, Solid initSolid shapeType) -> do
            s <- H.newShape b shapeType (H.Vector x y)
            initSolid s
            return s

    objectRef <- createObjectRef (body == Static) b shapes space
    init objectRef
    return objectRef


delete :: ObjectRef f -> IO ()
delete = deleteObjectRef
