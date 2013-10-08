module Physics.Hurl.Internal.Resource where

import Data.Monoid


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
