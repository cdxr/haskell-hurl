module Physics.Hurl.Space (
    -- * Spaces
    I.Space,
    I.newSpace,
    I.deleteSpace,
    I.stepSpace,

    -- ** Objects
    addObject,
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

import Data.Traversable ( Traversable )
import qualified Data.Traversable as Traversable

import Data.StateVar ( StateVar, ($=) )
import qualified Data.StateVar as StateVar

import qualified Physics.Hipmunk as H
import Foreign.C ( CInt )

import Physics.Hurl.Solid

import Physics.Hurl.Object

import Physics.Hurl.Internal.Space as I
import Physics.Hurl.Internal.ObjectRef
import Physics.Hurl.Internal.Utils


-- | Allocate an @Object f@ in the space. The `ObjectRef` returned is a
-- reference to the newly created Chipmunk entities in the space.
--
-- WARNING: The garbage collector will not automatically remove the
-- internal Chipmunk entities when it collects the ObjectRef. The user is
-- responsible for preventing memory leaks by calling deleteObject on
-- ObjectRefs.
addObject :: (Traversable f) => V2 Double -> Object f -> Space -> IO (ObjectRef f)
addObject (V2 x y) (Object body solids) space = do
    b <- case body of
        Static     -> H.newBody H.infinity H.infinity
        Body ma mo -> H.newBody ma mo
    H.position b $= H.Vector x y

    shapes <- Traversable.forM solids $ \(pos, s) -> do
        ref <- H.newShape b (shape s) (H.Vector x y)
        H.elasticity ref $= elasticity s
        H.friction   ref $= friction   s
        return ref

    createObjectRef (body == Static) b shapes space


-- | Delete all all Chipmunk entities referenced by an `ObjectRef`.
--
-- WARNING: All operations on an ObjectRef that has been deleted are
-- undefined.
deleteObject :: ObjectRef f -> IO ()
deleteObject = deleteObjectRef



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
