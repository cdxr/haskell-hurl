module Physics.Hurl.Internal.Utils where

import Linear

import Data.StateVar ( StateVar, ($=), makeStateVar )
import qualified Data.StateVar as StateVar

import qualified Physics.Hipmunk as H


-- | A synonym for Data.StateVar.get
getVar :: StateVar a -> IO a
getVar = StateVar.get

mapStateVar :: (a -> b) -> (b -> a) -> StateVar a -> StateVar b
mapStateVar f g v = makeStateVar (fmap f $ getVar v) ((v $=) . g)

vectorFromV2 :: V2 Double -> H.Vector
vectorFromV2 (V2 x y) = H.Vector x y

varVectorToV2 :: StateVar H.Vector -> StateVar (V2 Double)
varVectorToV2 = mapStateVar from vectorFromV2
  where
    from (H.Vector x y) = V2 x y


