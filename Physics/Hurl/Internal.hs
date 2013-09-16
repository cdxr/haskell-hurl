{-# LANGUAGE BangPatterns #-}

module Physics.Hurl.Internal where


import Control.Applicative

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

import Data.IORef

import qualified Physics.Hipmunk as H

import Data.Function ( on )

import Data.Foldable ( forM_ )


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


-- | Add `ObjectData` to the `Space`.
addObjectData :: ObjectData -> Space -> IO IntMap.Key
addObjectData od@(ObjectData b ss) (Space objectMap space)  = do
    -- add the resources to the Hipmunk Space
    H.spaceAdd space b
    forM_ ss $ H.spaceAdd space

    -- add the data to the map and return its new key
    insert od objectMap
  where
    insert :: a -> IORef (IntMap.Key, IntMap a) -> IO IntMap.Key
    insert a mapRef = atomicModifyIORef' mapRef $ \(key, imap) ->
        let !key' = key + 1
        in ((key', IntMap.insert key a imap), key)


-- | Delete `ObjectData` from the `Space`.
deleteObjectData :: IntMap.Key -> Space -> IO ()
deleteObjectData objectKey (Space objectMap space) = do
    -- remove the data from the map and return its value
    mData <- atomicModifyIORef' objectMap $ \(k, m) ->
        let (mData, !m') = deleteLookup objectKey m
        in ((k, m'), mData)

    case mData of
        Nothing -> return ()  -- user tried to delete non-existing data
        Just (ObjectData b ss) -> do
            -- free the resources from the Hipmunk Space
            H.spaceRemove space b
            forM_ ss $ H.spaceRemove space
  where
    deleteLookup :: IntMap.Key -> IntMap a -> (Maybe a, IntMap a)
    deleteLookup = IntMap.updateLookupWithKey (\_ _ -> Nothing)



-- The private Hipmunk data associated with an `Object` and stored in
-- a `Space`.
data ObjectData = ObjectData !H.Body ![H.Shape]
    deriving (Eq, Ord)


-- | An `Object` represents a body and any attached shapes in a `Space`.
newtype Object = Object { objectId :: IntMap.Key }
    deriving (Eq, Ord)





-- Utilities --------------------------------------------------------------

insert :: a -> IORef (IntMap.Key, IntMap a) -> IO IntMap.Key
insert a mapRef = atomicModifyIORef' mapRef $ \(thisId, imap) ->
    let !nextId = thisId + 1
    in ((nextId, IntMap.insert thisId a imap), thisId)

