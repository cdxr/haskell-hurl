module Physics.Hurl.Library where


import Physics.Hurl


-- | @ball r@ is an elastic ball of radius @r@.
ball :: Double -> Object'
ball = simpleObject 20 (Solid 0.9 0.5 $ Circle 0 20)
