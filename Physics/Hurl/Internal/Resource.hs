module Physics.Hurl.Internal.Resource where

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

import qualified Data.Traversable as Traversable

import Data.Monoid
import Data.IORef


-- | A @Resource e@ is a function from an environment @e@ to an allocator
-- and deallocator.
--
-- Resources are monoids. A `Resource` added on the right will be allocated
-- last and deallocated first.
newtype Resource e = Resource (e -> (IO (), IO ()))


instance Monoid (Resource e) where
    mempty = Resource $ \_ -> (return (), return ())
    Resource f `mappend` Resource g = Resource $ \e ->
        let (fa, fd) = f e
            (ga, gd) = g e
        in (fa >> ga, gd >> fd)


-- | Add a `Resource` with the given environment, and return its deallocator.
runResource :: Resource e -> e -> IO (IO ())
runResource (Resource r) e = let (add, remove) = r e in add >> return remove




data FinalizerMap = FM !IntMap.Key (IntMap (IO ()))

-- | An empty FinalizerMap
emptyFinalizerMap :: FinalizerMap
emptyFinalizerMap = FM 0 IntMap.empty

-- | Add a new finalizer to the `FinalizerMap`, returning a new map and the
-- new key.
insertFinalizer :: IO () -> FinalizerMap -> (FinalizerMap, IntMap.Key)
insertFinalizer fin (FM k imap) = (FM (k + 1) (IntMap.insert k fin imap), k)

-- | Run the finalizer with the corresponding `IntMap.Key`, and remove it
-- from the map. If no finalizer matches the key, the map is unmodified.
runFinalizer :: IntMap.Key -> IORef FinalizerMap -> IO ()
runFinalizer key finRef = do
    mFin <- atomicModifyIORef' finRef $ \(FM k imap) ->
        let (mFin, imap') = deleteLookup key imap
        in (FM k imap', mFin)
    Traversable.sequence mFin
    return ()
  where
    deleteLookup :: IntMap.Key -> IntMap a -> (Maybe a, IntMap a)
    deleteLookup = IntMap.updateLookupWithKey (\_ _ -> Nothing)


-- | Run a `Resource` initializer, add its finalizer to the `FinalizerMap`,
-- and return its key.
addResource :: Resource e -> e -> IORef FinalizerMap -> IO IntMap.Key
addResource r e mapRef = do
    fin <- runResource r e
    atomicModifyIORef' mapRef $ insertFinalizer fin
