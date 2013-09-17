{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Physics.Hurl.Internal.Space where


import Control.Applicative

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

import Data.IORef

import Data.Function ( on )
import Data.Foldable ( forM_ )
import Data.Maybe    ( fromMaybe )

import qualified Physics.Hipmunk as H


-- | A `Space` is a mutable reference to bodies and constraints in
-- a physical simulation. All operations on `Space` are in IO.
--
-- This is essentially the Hipmunk type `H.Space`
-- with additional bookkeeping.
data Space = Space 
    { finalizerMap :: !(IORef (IntMap.Key, IntMap (IO ())))
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


-- | An external key to hidden Hipmunk data
type ObjectKey = IntMap.Key

-- | The hidden Hipmunk data stored in a `Space`, associated to an
-- `ObjectKey`.
data ObjectData = ObjectData !H.Body [H.Shape] [H.StaticShape]
    deriving (Eq, Ord)

deriving instance Eq H.StaticShape
deriving instance Ord H.StaticShape


-- | Add `ObjectData` to the `Space`.
addObjectData :: ObjectData -> Space -> IO ObjectKey
addObjectData (ObjectData b ss stcs) (Space finalizers space)  = do
    -- add the resources to the Hipmunk Space
    H.spaceAdd space b
    forM_ ss $ H.spaceAdd space
    forM_ stcs $ H.spaceAdd space

    let remove = do
        forM_ ss $ H.spaceRemove space
        forM_ stcs $ H.spaceRemove space
        H.spaceRemove space b

    -- add the finalizer to the map and return its new key
    insert remove finalizers
  where
    insert :: a -> IORef (IntMap.Key, IntMap a) -> IO IntMap.Key
    insert a mapRef = atomicModifyIORef' mapRef $ \(key, imap) ->
        let !key' = key + 1
        in ((key', IntMap.insert key a imap), key)


-- | Delete `ObjectData` from the `Space`.
deleteObjectData :: ObjectKey -> Space -> IO ()
deleteObjectData objectKey (Space finalizers _) = do
    -- remove the data from the map and return its value
    mFin <- atomicModifyIORef' finalizers $ \(k, m) ->
        let (mData, !m') = deleteLookup objectKey m
        in ((k, m'), mData)

    -- mFin is Nothing when user tries to delete non-existing data
    -- should this raise an exception?
    fromMaybe (return ()) mFin
  where
    deleteLookup :: IntMap.Key -> IntMap a -> (Maybe a, IntMap a)
    deleteLookup = IntMap.updateLookupWithKey (\_ _ -> Nothing)
