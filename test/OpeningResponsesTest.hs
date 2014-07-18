module OpeningResponsesTest where

import BridgeBuddy.Cards
import BridgeBuddy.OpeningResponses

import Test.Framework                       (Test)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (assertEqual)

-- Balanced hand, 11 Points, => 2NT 
testOpeningResponseTo1NTA :: Hand
testOpeningResponseTo1NTA  = Hand {
    spades   = SuitHolding [King,Ten,Seven,Six,Four],
    hearts   = SuitHolding [Ace,Eight,Four],
    diamonds = SuitHolding [Jack,Ten],
    clubs    = SuitHolding [King,Nine,Seven]
}

-- Balanced hand, 10 Points, => Pass
testOpeningResponseTo1NTB :: Hand
testOpeningResponseTo1NTB  = Hand {
    spades   = SuitHolding [King,Ten,Seven,Six,Four],
    hearts   = SuitHolding [King,Eight,Four],
    diamonds = SuitHolding [Jack,Ten],
    clubs    = SuitHolding [King,Nine,Seven]
}


tests :: [Test]
tests = [     testResponseTo1NTA
            , testResponseTo1NTB
        ]

testResponseTo1NTA :: Test
testResponseTo1NTA  = testCase "testResponseTo1NTA" $ 
    assertEqual "Should respond to 1NT opening" (NT 2) (fst (openingResponse (NT 1) testOpeningResponseTo1NTA))

testResponseTo1NTB :: Test
testResponseTo1NTB  = testCase "testResponseTo1NTB" $ 
    assertEqual "Should respond to 1NT opening" Pass (fst (openingResponse (NT 1) testOpeningResponseTo1NTB))


