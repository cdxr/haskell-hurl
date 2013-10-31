{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- hunit.hs
--
-- HUnit tests for `hurl`
------------------------------------------------------------------------------

module Main where

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

assertSpace :: (Space -> IO a) -> Assertion
assertSpace f = do
    s <- newSpace
    _ <- f s
    deleteSpace s

case_space :: Assertion
case_space = assertSpace return

case_addObject :: Assertion
case_addObject = assertSpace $ \s -> do
    addObject Dynamic 0 basicSolids s
    addObject Static 0 basicSolids s

case_addDynamic :: Assertion
case_addDynamic = assertSpace $ addDynamic 0 basicSolids

case_addStatic :: Assertion
case_addStatic = assertSpace $ addStatic 0 basicSolids
