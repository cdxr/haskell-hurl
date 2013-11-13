{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- hunit.hs
--
-- HUnit tests for `hurl`
------------------------------------------------------------------------------

module Main where

import Control.Monad

import Test.HUnit
import Test.Tasty.HUnit
import Test.Tasty.TH

import Physics.Hurl
import Physics.Hurl.Internal.Space


main = $defaultMainGenerator


basicSolids :: [Solid]
basicSolids = map solid
    [ Circle 1 0
    , Segment 1 0 10
    , rectangle 800 50
    , Poly [0, V2 0 10, 10, V2 10 0]
    , Poly $ reverse [0, V2 0 10, 10, V2 10 0]
    ]

assertSpace :: (Space -> Assertion) -> Assertion
assertSpace f = do
    s <- newSpace
    _ <- f s
    deleteSpace s

-- fails if newSpace or deleteSpace throws an exception
case_newSpace_deleteSpace :: Assertion
case_newSpace_deleteSpace = assertSpace $ \_ -> return ()

-- fails if Chipmunk has not been automatically initialized
case_initializeChipmunk :: Assertion
case_initializeChipmunk = assertSpace $ \_ -> assert debugChipmunkInitialized

case_addObject :: Assertion
case_addObject = assertSpace $ \s -> void $ do
    addObject Dynamic 0 basicSolids s
    addObject Static 0 basicSolids s

case_addDynamic :: Assertion
case_addDynamic = assertSpace $ void . addDynamic 0 basicSolids

case_addStatic :: Assertion
case_addStatic = assertSpace $ void . addStatic 0 basicSolids


