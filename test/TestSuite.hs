module Main where

import Test.Framework (defaultMain, testGroup)

import CardsTest (tests)
import OpeningBidsTest (tests)
import OpeningResponsesTest (tests)

main :: IO ()
main = defaultMain allTests 
  where
    allTests = [ testGroup "Cards.Tests"
                        CardsTest.tests
            , testGroup "OpeningBids.Tests"
                        OpeningBidsTest.tests
            , testGroup "OpeningResponses.Tests"
                        OpeningResponsesTest.tests
            ] 

