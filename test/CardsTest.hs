module Main where

import Test.HUnit
import BridgeBuddy.Cards
import BridgeBuddy.OpeningBids

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

main :: IO ()
main = defaultMain $ hUnitTestToTests hunit_tests


hunit_tests :: Test.HUnit.Test
hunit_tests  = Test.HUnit.TestList [
      TestCase (assertEqual "test5CardMajorSpades"      (Trump Spades 1)   (fst (openingBid testHand5CardMajor)))
    , TestCase (assertEqual "testRule20_a"              (Trump Spades 1)   (fst (openingBid testHandRule20_a)))
    , TestCase (assertEqual "testRule20_b"              (Trump Spades 1)   (fst (openingBid testHandRule20_b)))
    , TestCase (assertEqual "testRule20_c"              (Trump Spades 1)   (fst (openingBid testHandRule20_c)))
    , TestCase (assertEqual "testRule20_d"              (Pass)             (fst (openingBid testHandRule20_d)))
    , TestCase (assertEqual "testSuit_a"                (Trump Hearts 1)   (fst (openingBid testHandSuit_a)))
    , TestCase (assertEqual "testSuit_b"                (Trump Hearts 1)   (fst (openingBid testHandSuit_b)))
    , TestCase (assertEqual "testSuit_c"                (Trump Hearts 1)   (fst (openingBid testHandSuit_c)))
    , TestCase (assertEqual "testSuit_d"                (Trump Diamonds 1) (fst (openingBid testHandSuit_d)))
    , TestCase (assertEqual "testSuit_e"                (Trump Diamonds 1) (fst (openingBid testHandSuit_e)))
    , TestCase (assertEqual "testSuit_f"                (Trump Clubs 1)    (fst (openingBid testHandSuit_f)))
    , TestCase (assertEqual "testSuit_g"                (Trump Diamonds 1) (fst (openingBid testHandSuit_g)))
    , TestCase (assertEqual "testSuit_h"                (Trump Hearts 1)   (fst (openingBid testHandSuit_h)))
    , TestCase (assertBool  "isPremptable_a"            (fst (isPremptable testHandPrempt_a)))
    , TestCase (assertEqual "playingTricks_a"           6 (playingTricks testHandRule20_a))
    , TestCase (assertEqual "strongOpening_a"           (Just Spades) (hasStrong2Opening testStrong2OpeningHand_a))
    , TestCase (assertEqual "strongOpening_b"           Nothing (hasStrong2Opening testStrong2OpeningHand_b))
    , TestCase (assertEqual "openingResponseTo1NT_a"    (NT 2) (fst (openingResponse (NT 1) testOpeningResponseTo1NT_a)))
    ]

testBalancedHand :: Hand
testBalancedHand = Hand {
    spades   = SuitHolding [Ace, King, Nine, Seven],
    hearts   = SuitHolding [Queen, Five, Two],
    diamonds = SuitHolding [King, Nine, Five],
    clubs    = SuitHolding [Jack, Six, Three]
    }

testUnbalancedHand :: Hand
testUnbalancedHand = Hand {
    spades   = SuitHolding [Ace, King, Nine, Seven, Five],
    hearts   = SuitHolding [Queen, Five, Two],
    diamonds = SuitHolding [King],
    clubs    = SuitHolding [Jack, Six, Four, Three]
    }

-- Test data
testHand5CardMajor :: Hand
testHand5CardMajor = Hand {
    spades   = SuitHolding [Ace, King, Nine, Seven, Five],
    hearts   = SuitHolding [Queen, Five, Two],
    diamonds = SuitHolding [King, Nine],
    clubs    = SuitHolding [Jack, Six, Three]
}

-- Should pass
testHandRule20_a :: Hand
testHandRule20_a = Hand {
    spades   = SuitHolding [King, Queen, Nine, Seven, Five],
    hearts   = SuitHolding [Queen, Jack, Five, Four, Two],
    diamonds = SuitHolding [Queen],
    clubs    = SuitHolding [Nine, Six]
}

-- Should pass
testHandRule20_b :: Hand
testHandRule20_b = Hand {
    spades   = SuitHolding [King, Queen, Nine, Seven, Five],
    hearts   = SuitHolding [King, Jack, Five, Four],
    diamonds = SuitHolding [Queen, Nine],
    clubs    = SuitHolding [Nine, Six]
}

-- Should pass
testHandRule20_c :: Hand
testHandRule20_c = Hand {
    spades   = SuitHolding [King, Queen, Nine, Seven, Five, Two],
    hearts   = SuitHolding [King, Jack, Five],
    diamonds = SuitHolding [Queen],
    clubs    = SuitHolding [Nine, Six, Three]
}

-- Should fail
testHandRule20_d :: Hand
testHandRule20_d = Hand {
    spades   = SuitHolding [King, Queen, Nine, Seven, Five, Two],
    hearts   = SuitHolding [King, Jack, Five],
    diamonds = SuitHolding [Jack],
    clubs    = SuitHolding [Nine, Six, Three]
}


-- Test suit biddng with a 4432 shape - 4 spades, 4 hearts
testHandSuit_a :: Hand
testHandSuit_a  = Hand {
    spades   = SuitHolding [King, Queen, Jack, Eight],
    hearts   = SuitHolding [King, Jack, Five, Two],
    diamonds = SuitHolding [Jack, Six],
    clubs    = SuitHolding [Ace, Six, Three]
}

-- Test suit biddng with a 4432 shape - 4 hearts, 4 Clubs
testHandSuit_b :: Hand
testHandSuit_b  = Hand {
    spades   = SuitHolding [Ace, Queen, Nine],
    hearts   = SuitHolding [King, Jack, Five, Two],
    diamonds = SuitHolding [Jack, Six],
    clubs    = SuitHolding [Ace, Six, Three, Two]
}

-- Test suit biddng with a 5422 shape
testHandSuit_c :: Hand
testHandSuit_c  = Hand {
    spades   = SuitHolding [King, Queen, Nine, Seven],
    hearts   = SuitHolding [King, Jack, Five, Three, Two],
    diamonds = SuitHolding [Jack, Six],
    clubs    = SuitHolding [Ace, Six]
}

-- Test suit biddng with a 5521 shape
testHandSuit_d :: Hand
testHandSuit_d  = Hand {
    spades   = SuitHolding [King],
    hearts   = SuitHolding [King, Jack],
    diamonds = SuitHolding [Jack, Six, Five, Three, Two],
    clubs    = SuitHolding [Ace, King, Five, Three, Two]
}


-- Test suit biddng with a 4441 shape with a red suit heart singleton => Diamond bid
testHandSuit_e :: Hand
testHandSuit_e  = Hand {
    spades   = SuitHolding [King, Queen, Nine, Seven],
    hearts   = SuitHolding [King],
    diamonds = SuitHolding [Jack, Six, Five, Three],
    clubs    = SuitHolding [Ace, King, Five, Three]
}

-- Test suit biddng with a 4441 shape with a red suit diamond singleton => Club bid
testHandSuit_f :: Hand
testHandSuit_f  = Hand {
    spades   = SuitHolding [King, Queen, Nine, Seven],
    hearts   = SuitHolding [King, Six, Five, Three],
    diamonds = SuitHolding [Jack],
    clubs    = SuitHolding [Ace, King, Five, Three]
}


-- Test suit biddng with a 4441 shape with a black suit spade singleton => Diamond bid
testHandSuit_g :: Hand
testHandSuit_g  = Hand {
    spades   = SuitHolding [King],
    hearts   = SuitHolding [King, Jack, Three, Two],
    diamonds = SuitHolding [Jack, Six, Five, Three],
    clubs    = SuitHolding [Ace, King, Five, Three]
}

-- Test suit biddng with a 4441 shape with a black suit club singleton => Heart bid
testHandSuit_h :: Hand
testHandSuit_h  = Hand {
    spades   = SuitHolding [King, Queen, Nine, Seven],
    hearts   = SuitHolding [King, Jack, Three, Two],
    diamonds = SuitHolding [Jack, Six, Five, Three],
    clubs    = SuitHolding [Ace]
}

-- Premptable bid
testHandPrempt_a :: Hand
testHandPrempt_a  = Hand {
    spades   = SuitHolding [King, Queen, Nine, Seven, Six, Five, Four],
    hearts   = SuitHolding [King, Jack, Three],
    diamonds = SuitHolding [Seven, Six],
    clubs    = SuitHolding [Five]
}

-- This hand has 19 points and >= 2 honours in the long suit
testStrong2OpeningHand_a :: Hand
testStrong2OpeningHand_a = Hand {
    spades   = SuitHolding [Ace, King, Queen, Ten, Eight, Three],
    hearts   = SuitHolding [King, Queen, Jack, Ten],
    diamonds = SuitHolding [Ace, Six, Three],
    clubs    = SuitHolding []
}

-- This hand has 16 points but ony 1 honour in spades
testStrong2OpeningHand_b :: Hand
testStrong2OpeningHand_b = Hand {
    spades   = SuitHolding [Queen, Ten, Nine, Eight, Five, Three],
    hearts   = SuitHolding [Ace, King, Queen, Jack, Ten],
    diamonds = SuitHolding [Ace, Three],
    clubs    = SuitHolding []
}
-- This hand has 16 points but ony 1 honour in spades
testFailingStrong2OpeningHand_a :: Hand
testFailingStrong2OpeningHand_a  = Hand {
    spades   = SuitHolding [Ace,Queen,Jack,Ten,Seven,Five,Two],
    hearts   = SuitHolding [Ace],
    diamonds = SuitHolding [],
    clubs    = SuitHolding [Ace,King,Nine,Seven,Two]
}

-- Balanced hand, 11 Points, => 2NT 
testOpeningResponseTo1NT_a :: Hand
testOpeningResponseTo1NT_a  = Hand {
    spades   = SuitHolding [King,Ten,Seven,Six,Four],
    hearts   = SuitHolding [Ace,Eight,Four],
    diamonds = SuitHolding [Jack,Ten],
    clubs    = SuitHolding [King,Nine,Seven]
}

-- Balanced hand, 10 Points, => Pass
testOpeningResponseTo1NT_b :: Hand
testOpeningResponseTo1NT_b  = Hand {
    spades   = SuitHolding [King,Ten,Seven,Six,Four],
    hearts   = SuitHolding [King,Eight,Four],
    diamonds = SuitHolding [Jack,Ten],
    clubs    = SuitHolding [King,Nine,Seven]
}

{-
-- A sample table
testTable_a :: TableHands
testTable_a = [
(North, [Six, Seven, Nine, Ace, Two, Three, Six, Three, Ten, Jack, Three, Six, King,
(East,  [Five, Jack, Four, Seven, Nine, Jack, Five, Six, Nine, Five, Seven, Nine, Jack,
(South, [Two, Eight, Five, Eight, Ten, King, Ace, Four, Eight, Queen, Two, Ten, Queen,
(West,  [Three, Four, Ten, Queen, King, Queen, Two, Seven, King, Ace, Four, Eight, Ace)
]

-}
