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


main = $defaultMainGenerator


basicSolids :: [Solid]
basicSolids = map solid
    [ Circle 1 0
    , Segment 1 0 10
    , Poly [0, V2 0 10, 10, V2 10 0]
    ]

assertSpace :: (Space -> IO a) -> Assertion
assertSpace f = do
    s <- newSpace
    f s
    deleteSpace s

case_space :: Assertion
case_space = assertSpace return

case_single_dynamic :: Assertion
case_single_dynamic = assertSpace $ addDynamic 0 basicSolids

case_single_static :: Assertion
case_single_static = assertSpace $ addStatic 0 basicSolids
