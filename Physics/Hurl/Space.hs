module Physics.Hurl.Space (
    -- * Spaces
    I.Space,
    I.newSpace,
    I.deleteSpace,
    I.stepSpace,

    -- ** Objects
    addDynamic,
    addStatic,
    deleteObject,

    -- ** Properties
    -- | These properties are taken directly from Hipmunk for the time
    -- being.
    getTimeStamp,
    iterations,
    damping,
    gravity,
    resizeStaticHash,
    resizeActiveHash,
    setDefaultCollisionHandler,
)
where

import Linear.V2

import Data.StateVar ( StateVar )
import qualified Data.StateVar as StateVar

import qualified Physics.Hipmunk as H
import Foreign.C ( CInt )

import Physics.Hurl.Internal.Space as I
import Physics.Hurl.Internal.Utils


-- TODO
--  replace setDefaultCollisionHandler with new functionality
--  point query


getTimeStamp :: Space -> IO CInt
getTimeStamp = StateVar.get . H.timeStamp . hipmunkSpace

iterations :: Space -> StateVar CInt
iterations = H.iterations . hipmunkSpace

damping :: Space -> StateVar Double
damping = H.damping . hipmunkSpace

gravity :: Space -> StateVar (V2 Double)
gravity = varVectorToV2 . H.gravity . hipmunkSpace

resizeStaticHash :: Double -> CInt -> Space -> IO ()
resizeStaticHash cellSize minCells s =
    H.resizeStaticHash (hipmunkSpace s) cellSize minCells

resizeActiveHash :: Double -> CInt -> Space -> IO ()
resizeActiveHash cellSize minCells s =
    H.resizeActiveHash (hipmunkSpace s) cellSize minCells



setDefaultCollisionHandler :: H.CollisionHandler -> Space -> IO ()
setDefaultCollisionHandler h s = H.setDefaultCollisionHandler (hipmunkSpace s) h
