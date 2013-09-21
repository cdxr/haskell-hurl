module Physics.Hurl.Internal.ObjectRef where

import Control.Applicative
import Control.Monad
import Linear.V2

import Data.Foldable ( Foldable, foldMap, toList )
import Data.Monoid

import qualified Physics.Hipmunk as H

import Physics.Hurl.Internal.Resource
import Physics.Hurl.Internal.Space


type SolidRef = H.Shape


-- | A reference to a physical entity in a `Space`.
data ObjectRef f = ObjectRef
    { objectSpace     :: Space
    , objectBody      :: H.Body
    , objectSolids    :: f SolidRef
    , objectFinalizer :: IO ()
    }


-- | Create an object and return its `ObjectRef`.
createObjectRef
    :: (Foldable f)
    => Bool        -- ^ make a static object?
    -> H.Body      -- ^ Hipmunk body
    -> f H.Shape   -- ^ Hipmunk shapes
    -> Space
    -> IO (ObjectRef f)
createObjectRef static b ss space = ObjectRef space b ss <$> runResource r space
  where
    r :: Resource Space
    r | static    = foldMap (spaceResource . H.Static) ss
      | otherwise = spaceResource b <> foldMap spaceResource ss


-- | Delete an `ObjectRef`.
deleteObjectRef :: ObjectRef f -> IO ()
deleteObjectRef = objectFinalizer


-- | Determine if a point intersects with a solid.
querySolid :: V2 Double -> SolidRef -> IO Bool
querySolid (V2 x y) s = H.shapePointQuery s (H.Vector x y)

-- | Find all solids in the given object that contain the point.
queryObject :: (Foldable f) => V2 Double -> ObjectRef f -> IO [SolidRef]
queryObject p = filterM (querySolid p) . toList . objectSolids
