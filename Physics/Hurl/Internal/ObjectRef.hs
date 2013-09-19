module Physics.Hurl.Internal.ObjectRef where

import Control.Applicative

import Data.Foldable ( Foldable, foldMap )
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
createObjectRef static b ss space =
    let r = spaceResource b <> foldMap mkShape ss
    in ObjectRef space b ss <$> runResource r space
  where
    mkShape :: H.Shape -> Resource Space
    mkShape
      | static    = spaceResource . H.Static
      | otherwise = spaceResource


-- | Delete an `ObjectRef`.
deleteObjectRef :: ObjectRef f -> IO ()
deleteObjectRef = objectFinalizer

