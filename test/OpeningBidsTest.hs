module OpeningBidsTest where

import BridgeBuddy.Cards
import BridgeBuddy.OpeningBids

import Test.Framework                       (Test)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (assertBool, assertEqual)


tests :: [Test]
tests = [     testRule20A
            , testRule20B
            , testRule20C
            , testRule20D
            , testStrong2OpeningA
            , testStrong2OpeningB
            , testSuitA
            , testSuitB
            , testSuitC
            , testSuitD
            , testSuitE
            , testSuitF
            , testSuitG
            , testSuitH
            , testPremptA
            , testPremptB
        ]

testRule20A :: Test
testRule20A = testCase "testRule20A" $ do
    let hand = Hand {
        spades   = SuitHolding [King, Queen, Nine, Seven, Five],
        hearts   = SuitHolding [Queen, Jack, Five, Four, Two],
        diamonds = SuitHolding [Queen],
        clubs    = SuitHolding [Nine, Six]
    }
    assertEqual "Hand should pass Rule of 20" (Trump Spades 1) (fst (openingBid hand))

testRule20B :: Test
testRule20B = testCase "testRule20B" $ do
    let hand = Hand {
        spades   = SuitHolding [King, Queen, Nine, Seven, Five],
        hearts   = SuitHolding [King, Jack, Five, Four],
        diamonds = SuitHolding [Queen, Nine],
        clubs    = SuitHolding [Nine, Six]
    }
    assertEqual "Hand should pass Rule of 20" (Trump Spades 1) (fst (openingBid hand))

testRule20C :: Test
testRule20C = testCase "testRule20C" $ do
    let hand = Hand {
        spades   = SuitHolding [King, Queen, Nine, Seven, Five, Two],
        hearts   = SuitHolding [King, Jack, Five],
        diamonds = SuitHolding [Queen],
        clubs    = SuitHolding [Nine, Six, Three]
    }
    assertEqual "Hand should pass Rule of 20" (Trump Spades 1) (fst (openingBid hand))

testRule20D :: Test
testRule20D = testCase "testRule20D" $ do
    let hand = Hand {
        spades   = SuitHolding [King, Queen, Nine, Seven, Five, Two],
        hearts   = SuitHolding [King, Jack, Five],
        diamonds = SuitHolding [Jack],
        clubs    = SuitHolding [Nine, Six, Three]
    }
    assertEqual "Hand should fail Rule of 20" Pass (fst (openingBid hand))


testSuitA :: Test
testSuitA = testCase "testSuitA" $ do
    let hand  = Hand {
        spades   = SuitHolding [King, Queen, Jack, Eight],
        hearts   = SuitHolding [King, Jack, Five, Two],
        diamonds = SuitHolding [Jack, Six],
        clubs    = SuitHolding [Ace, Six, Three]
    }
    assertEqual "Test suit biddng with a 4432 shape - 4 spades, 4 hearts" (Trump Hearts 1) (fst (openingBid hand))
 
testSuitB :: Test
testSuitB = testCase "testSuitB" $ do
    let hand  = Hand {
        spades   = SuitHolding [Ace, Queen, Nine],
        hearts   = SuitHolding [King, Jack, Five, Two],
        diamonds = SuitHolding [Jack, Six],
        clubs    = SuitHolding [Ace, Six, Three, Two]
    }
    assertEqual "Test suit biddng with a 4432 shape - 4 hearts, 4 Clubs" (Trump Hearts 1) (fst (openingBid hand))

testSuitC :: Test
testSuitC = testCase "testSuitC" $ do
    let hand  = Hand {
        spades   = SuitHolding [King, Queen, Nine, Seven],
        hearts   = SuitHolding [King, Jack, Five, Three, Two],
        diamonds = SuitHolding [Jack, Six],
        clubs    = SuitHolding [Ace, Six]
    }
    assertEqual "Test suit biddng with a 5422 shape" (Trump Hearts 1) (fst (openingBid hand))

testSuitD :: Test
testSuitD = testCase "testSuitD" $ do
    let hand  = Hand {
        spades   = SuitHolding [King],
        hearts   = SuitHolding [King, Jack],
        diamonds = SuitHolding [Jack, Six, Five, Three, Two],
        clubs    = SuitHolding [Ace, King, Five, Three, Two]
    }
    assertEqual "Test suit biddng with a 5521 shape" (Trump Diamonds 1) (fst (openingBid hand))

testSuitE :: Test
testSuitE = testCase "testSuitE" $ do
    let hand  = Hand {
        spades   = SuitHolding [King, Queen, Nine, Seven],
        hearts   = SuitHolding [King],
        diamonds = SuitHolding [Jack, Six, Five, Three],
        clubs    = SuitHolding [Ace, King, Five, Three]
    }
    assertEqual "Test suit biddng with a 4441 shape with a heart singleton => Diamond bid" (Trump Diamonds 1) (fst (openingBid hand))

testSuitF :: Test
testSuitF = testCase "testSuitF" $ do
    let hand  = Hand {
        spades   = SuitHolding [King, Queen, Nine, Seven],
        hearts   = SuitHolding [King, Six, Five, Three],
        diamonds = SuitHolding [Jack],
        clubs    = SuitHolding [Ace, King, Five, Three]
    }
    assertEqual "Test suit biddng with a 4441 shape with a diamond singleton => Club bid" (Trump Clubs 1) (fst (openingBid hand))

testSuitG :: Test
testSuitG = testCase "testSuitG" $ do
    let hand  = Hand {
        spades   = SuitHolding [King],
        hearts   = SuitHolding [King, Jack, Three, Two],
        diamonds = SuitHolding [Jack, Six, Five, Three],
        clubs    = SuitHolding [Ace, King, Five, Three]
    }
    assertEqual "Test suit biddng with a 4441 shape with a spade singleton => Diamond bid" (Trump Diamonds 1) (fst (openingBid hand))

testSuitH :: Test
testSuitH = testCase "testSuitH" $ do
    let hand  = Hand {
        spades   = SuitHolding [King, Queen, Nine, Seven],
        hearts   = SuitHolding [King, Jack, Three, Two],
        diamonds = SuitHolding [Jack, Six, Five, Three],
        clubs    = SuitHolding [Ace]
    }
    assertEqual "Test suit biddng with a 4441 shape with a club singleton => Heart bid" (Trump Hearts 1) (fst (openingBid hand))

testStrong2OpeningA :: Test
testStrong2OpeningA = testCase "testStrong2OpeningA" $ do
    let hand = Hand {
        spades   = SuitHolding [Ace, King, Queen, Ten, Eight, Three],
        hearts   = SuitHolding [King, Queen, Jack, Ten],
        diamonds = SuitHolding [Ace, Six, Three],
        clubs    = SuitHolding []
    }
    assertEqual "Test hand with 19 points and >= 2 honours in the 6-card suit" (Trump Spades 2) (fst $ openingBid hand)

testStrong2OpeningB :: Test
testStrong2OpeningB = testCase "testStrong2OpeningA" $ do
    let hand = Hand {
        spades   = SuitHolding [Queen, Ten, Nine, Eight, Five, Three],
        hearts   = SuitHolding [Ace, King, Queen, Jack, Ten],
        diamonds = SuitHolding [Ace, Three],
        clubs    = SuitHolding []
    }
    assertEqual "This hand has 16 points but ony 1 honour in spades" (Trump Spades 1) (fst $ openingBid hand)


testPremptA :: Test
testPremptA = testCase "testPremptA" $ do
    let hand = Hand {
        spades   = SuitHolding [King, Queen, Nine, Seven, Six, Five, Four],
        hearts   = SuitHolding [King, Jack, Three],
        diamonds = SuitHolding [Seven, Six],
        clubs    = SuitHolding [Five]
    }
    assertEqual "Hand should be pre-emptable" (Trump Spades 3) (fst (openingBid hand))

testPremptB :: Test
testPremptB = testCase "testPremptB" $ do
    let hand = Hand {
        spades   = SuitHolding [King, Queen, Nine, Seven, Six, Five, Four],
        hearts   = SuitHolding [King, Jack, Three],
        diamonds = SuitHolding [Seven, Six],
        clubs    = SuitHolding [Ace]
    }
    assertEqual "Hand should NOT be pre-emptable" (Trump Spades 1) (fst (openingBid hand))
