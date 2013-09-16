{-# LANGUAGE BangPatterns #-}

module Physics.Hurl.Internal where

import Linear

import Control.Monad
import Control.Applicative

import Data.Bifunctor ( second )

import Data.StateVar ( StateVar, ($=), ($=!), makeStateVar )
import qualified Data.StateVar as StateVar

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

import Data.IORef

import qualified Physics.Hipmunk as H

import Data.Function ( on )


-- | A `Space` is a mutable reference to bodies and constraints in
-- a physical simulation. All operations on `Space` are in IO.
--
-- This is essentially the Hipmunk type `H.Space`
-- with additional bookkeeping.
data Space = Space 
    { objectMap    :: !(IORef (IntMap.Key, IntMap ObjectData))
    , hipmunkSpace :: !H.Space
    }

instance Eq Space where
    (==) = (==) `on` hipmunkSpace

instance Ord Space where
    compare = compare `on` hipmunkSpace
    

-- | Create a new empty `Space`.
newSpace :: IO Space
newSpace = Space <$> newIORef (0, IntMap.empty) <*> H.newSpace


-- todo: add a finalizer to perform this automatically

-- | Delete a `Space`. All operations on a deleted `Space` are undefined.
deleteSpace :: Space -> IO ()
deleteSpace = H.freeSpace . hipmunkSpace

-- | @stepSpace delta space@ will simulate @space@ for a @delta@ timestep.
-- It is necessary to perform this simulation at a fixed @delta@ to ensure
-- accuracy.
stepSpace :: Double -> Space -> IO ()
stepSpace delta space = H.step (hipmunkSpace space) (realToFrac delta)


-- | Add an object to the `Space`.
addObject :: ObjectData -> Space -> IO Object
addObject od = fmap Object . insert od . objectMap

-- | Delete an object from the `Space`.
deleteObject :: Object -> Space -> IO ()
deleteObject (Object objectId) space =
    modifyIORef' (objectMap space) $ second $ IntMap.delete objectId






-- The private Hipmunk data associated with an `Object` and stored in
-- a `Space`.
data ObjectData = ObjectData !H.Body ![H.Shape]
    deriving (Eq, Ord)


-- | An `Object` represents a body and any attached shapes in a `Space`.
newtype Object = Object { objectId :: IntMap.Key }
    deriving (Eq, Ord)





-- Utilities --------------------------------------------------------------

insert :: a -> IORef (IntMap.Key, IntMap a) -> IO IntMap.Key
insert a mapRef = do
    (thisId, imap) <- readIORef mapRef
    let !nextId = thisId + 1
    writeIORef mapRef (nextId, IntMap.insert thisId a imap)
    return thisId


-- | A synonym for Data.StateVar.get
getVar :: StateVar a -> IO a
getVar = StateVar.get

mapStateVar :: (a -> b) -> (b -> a) -> StateVar a -> StateVar b
mapStateVar f g v = makeStateVar (f <$> getVar v) ((v $=) . g)

vectorFromV2 :: V2 Double -> H.Vector
vectorFromV2 (V2 x y) = H.Vector x y

varVectorToV2 :: StateVar H.Vector -> StateVar (V2 Double)
varVectorToV2 = mapStateVar from vectorFromV2
  where
    from (H.Vector x y) = V2 x y

