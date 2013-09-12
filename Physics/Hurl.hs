{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Physics.Hurl
(
  withPhysics
-- * Space 
, Space
, runSpace
, MonadSpace
, stepSpace
, damping
, gravity

-- * Objects
, Mass
, Moment
, Object (..)
, createObject
, destroyObject
, Shape (..)
, position
, angle
, velocity
, applyForce

-- * StateVar
, getVar
, ( $= )
, ( $=! )
)

where

import Linear

import Control.Monad
import Control.Monad.Reader
import Control.Applicative

import Control.Monad.Catch

import Data.StateVar ( StateVar, ($=), ($=!), makeStateVar )
import qualified Data.StateVar as StateVar

import qualified Physics.Hipmunk as H


-- | A synonym for Data.StateVar.get
getVar :: StateVar a -> IO a
getVar = StateVar.get


withPhysics :: IO a -> IO a
withPhysics = (H.initChipmunk >>)


newtype Space m a = Space (ReaderT H.Space m a)
    deriving (Functor, Applicative, Monad, MonadTrans,
              MonadReader H.Space, MonadIO)

type MonadSpace m = (MonadIO m, MonadReader H.Space m)


-- | Allocate a new Hipmunk 'H.Space'
runSpace :: (MonadCatch m, MonadIO m) => Space m a -> m a
runSpace (Space m) = bracket (liftIO H.newSpace) (liftIO . H.freeSpace) (runReaderT m)


stepSpace :: (MonadSpace m) => Double -> m ()
stepSpace dt = do
    s <- ask
    liftIO $ H.step s dt


damping :: (MonadSpace m) => m (StateVar Double)
damping = liftM H.damping ask


gravity :: (MonadSpace m) => m (StateVar (V2 Double))
gravity = liftM (fromVectorVar . H.gravity) ask


type Mass = Double
type Moment = Double

data Shape
    = Circle Double
    | Rect Double Double
    deriving (Show, Read, Eq, Ord)

mkShapeType :: Shape -> H.ShapeType
mkShapeType s = case s of
    Circle r -> H.Circle r
    Rect w h -> H.Polygon $ map (uncurry H.Vector)
                    [(-x, -y), (x, -y), (x, y), (-x, y)]
      where x = w/2
            y = h/2


data Object = Object
    { objBody :: !H.Body
    , objShapes :: [H.Shape]
    }

createObject :: (MonadSpace m) => Mass -> Moment -> Shape -> m Object
createObject bmass bmoment shape = do
    space <- ask
    liftIO $ do
        body <- H.newBody bmass bmoment
        hshape <- H.newShape body (mkShapeType shape) $ H.Vector 0 0
        H.spaceAdd space body
        H.spaceAdd space hshape
        return $ Object body [hshape]

destroyObject :: (MonadSpace m) => Object -> m ()
destroyObject (Object body shapes) = do
    space <- ask
    liftIO $ do
        H.spaceRemove space body
        mapM_ (H.spaceRemove space) shapes

--_vector :: Iso' V2 H.Vector

position :: Object -> StateVar (V2 Double)
position = fromVectorVar . H.position . objBody

velocity :: Object -> StateVar (V2 Double)
velocity = fromVectorVar . H.velocity . objBody

angle :: Object -> StateVar Double
angle = mapStateVar realToFrac realToFrac . H.angle . objBody

applyForce :: V2 Double -> Object -> IO ()
applyForce v o = H.applyOnlyForce b (vectorFromV2 v)
             =<< getVar (H.position b)
  where
    b = objBody o



mapStateVar :: (a -> b) -> (b -> a) -> StateVar a -> StateVar b
mapStateVar f g v = makeStateVar (f <$> getVar v) ((v $=) . g)

vectorFromV2 :: V2 Double -> H.Vector
vectorFromV2 (V2 x y) = H.Vector x y

fromVectorVar :: StateVar H.Vector -> StateVar (V2 Double)
fromVectorVar = mapStateVar from vectorFromV2
  where
    from (H.Vector x y) = V2 x y



setDefaultHandler :: (MonadSpace m) => H.CollisionHandler -> m ()
setDefaultHandler h = do
    s <- ask
    liftIO $ H.setDefaultCollisionHandler s h
