module CardsTest where

import Test.HUnit
import Cards

tests :: Test
tests = TestList [
      TestCase (assertEqual "test5CardMajorSpades" (Trump Spades 1)   (fst (openingBid testHand5CardMajor)))
    , TestCase (assertEqual "testRule20_a"         (Trump Spades 1)   (fst (openingBid testHandRule20_a)))
    , TestCase (assertEqual "testRule20_b"         (Trump Spades 1)   (fst (openingBid testHandRule20_b)))
    , TestCase (assertEqual "testRule20_c"         (Trump Spades 1)   (fst (openingBid testHandRule20_c)))
    , TestCase (assertEqual "testRule20_d"         (Pass)            (fst (openingBid testHandRule20_d)))
    , TestCase (assertEqual "testSuit_a"           (Trump Hearts 1)   (fst (openingBid testHandSuit_a)))
    , TestCase (assertEqual "testSuit_b"           (Trump Hearts 1)   (fst (openingBid testHandSuit_b)))
    , TestCase (assertEqual "testSuit_c"           (Trump Hearts 1)   (fst (openingBid testHandSuit_c)))
    , TestCase (assertEqual "testSuit_d"           (Trump Diamonds 1) (fst (openingBid testHandSuit_d)))
    , TestCase (assertEqual "testSuit_e"           (Trump Diamonds 1) (fst (openingBid testHandSuit_e)))
    , TestCase (assertEqual "testSuit_f"           (Trump Clubs 1)    (fst (openingBid testHandSuit_f)))
    , TestCase (assertEqual "testSuit_g"           (Trump Diamonds 1) (fst (openingBid testHandSuit_g)))
    , TestCase (assertEqual "testSuit_h"           (Trump Hearts 1)   (fst (openingBid testHandSuit_h)))
    , TestCase (assertBool "isPremptable_a"        (fst (isPremptable testHandPrempt_a)))
    ]

-- Test data 
testHand5CardMajor :: Hand
testHand5CardMajor = [(Spades, Ace), (Spades, King), (Spades, Nine), (Spades, Seven), (Spades, Five), (Hearts, Queen), (Hearts, Five), (Hearts, Two), (Diamonds, King), (Diamonds, Nine), (Clubs, Jack), (Clubs, Six), (Clubs, Three)]

-- Should pass
testHandRule20_a :: Hand
testHandRule20_a = [(Spades, King), (Spades, Queen), (Spades, Nine), (Spades, Seven), (Spades, Five),
                    (Hearts, Queen), (Hearts, Jack), (Hearts, Five), (Hearts, Four), (Hearts, Two),
                    (Diamonds, Queen), 
                    (Clubs, Nine), (Clubs, Six)]

-- Should pass
testHandRule20_b :: Hand
testHandRule20_b = [(Spades, King), (Spades, Queen), (Spades, Nine), (Spades, Seven), (Spades, Five),
                    (Hearts, King), (Hearts, Jack), (Hearts, Five), (Hearts, Four),
                    (Diamonds, Queen), (Diamonds, Nine),
                    (Clubs, Nine), (Clubs, Six)]

-- Should pass
testHandRule20_c :: Hand
testHandRule20_c = [(Spades, King), (Spades, Queen), (Spades, Nine), (Spades, Seven), (Spades, Five), (Spades, Two),
                    (Hearts, King), (Hearts, Jack), (Hearts, Five),
                    (Diamonds, Queen), 
                    (Clubs, Nine), (Clubs, Six), (Clubs, Three)]

-- Should fail
testHandRule20_d :: Hand
testHandRule20_d = [(Spades, King), (Spades, Queen), (Spades, Nine), (Spades, Seven), (Spades, Five), (Spades, Two),
                    (Hearts, King), (Hearts, Jack), (Hearts, Five),
                    (Diamonds, Jack), 
                    (Clubs, Nine), (Clubs, Six), (Clubs, Three)]


-- Test suit biddng with a 4432 shape - 4 spades, 4 hearts
testHandSuit_a :: Hand
testHandSuit_a  = [(Spades, King), (Spades, Queen), (Spades, Jack), (Spades, Eight),
                   (Hearts, King), (Hearts, Jack), (Hearts, Five), (Hearts, Two),
                   (Diamonds, Jack), (Diamonds, Six), 
                   (Clubs, Ace), (Clubs, Six), (Clubs, Three)]

-- Test suit biddng with a 4432 shape - 4 hearts, 4 Clubs
testHandSuit_b :: Hand
testHandSuit_b  = [(Spades, Ace), (Spades, Queen), (Spades, Nine),
                   (Hearts, King), (Hearts, Jack), (Hearts, Five), (Hearts, Two),
                   (Diamonds, Jack), (Diamonds, Six), 
                   (Clubs, Ace), (Clubs, Six), (Clubs, Three), (Clubs, Two)]

-- Test suit biddng with a 5422 shape
testHandSuit_c :: Hand
testHandSuit_c  = [(Spades, King), (Spades, Queen), (Spades, Nine), (Spades, Seven),
                   (Hearts, King), (Hearts, Jack), (Hearts, Five), (Hearts, Three), (Hearts, Two),
                   (Diamonds, Jack), (Diamonds, Six),
                   (Clubs, Ace), (Clubs, Six)]

-- Test suit biddng with a 5521 shape
testHandSuit_d :: Hand
testHandSuit_d  = [(Spades, King), 
                   (Hearts, King), (Hearts, Jack), 
                   (Diamonds, Jack), (Diamonds, Six), (Diamonds, Five), (Diamonds, Three), (Diamonds, Two),
                   (Clubs, Ace), (Clubs, King), (Clubs, Five), (Clubs, Three), (Clubs, Two)]


-- Test suit biddng with a 4441 shape with a red suit heart singleton => Diamond bid
testHandSuit_e :: Hand
testHandSuit_e  = [(Spades, King), (Spades, Queen), (Spades, Nine), (Spades, Seven),
                   (Hearts, King), 
                   (Diamonds, Jack), (Diamonds, Six), (Diamonds, Five), (Diamonds, Three),
                   (Clubs, Ace), (Clubs, King), (Clubs, Five), (Clubs, Three)]

-- Test suit biddng with a 4441 shape with a red suit diamond singleton => Club bid
testHandSuit_f :: Hand
testHandSuit_f  = [(Spades, King), (Spades, Queen), (Spades, Nine), (Spades, Seven),
                   (Hearts, King), (Hearts, Six), (Hearts, Five), (Hearts, Three),
                   (Diamonds, Jack), 
                   (Clubs, Ace), (Clubs, King), (Clubs, Five), (Clubs, Three)]


-- Test suit biddng with a 4441 shape with a black suit spade singleton => Diamond bid
testHandSuit_g :: Hand
testHandSuit_g  = [(Spades, King), 
                   (Hearts, King), (Hearts, Jack), (Hearts, Three), (Hearts, Two),
                   (Diamonds, Jack), (Diamonds, Six), (Diamonds, Five), (Diamonds, Three),
                   (Clubs, Ace), (Clubs, King), (Clubs, Five), (Clubs, Three)]

-- Test suit biddng with a 4441 shape with a black suit club singleton => Heart bid
testHandSuit_h :: Hand
testHandSuit_h  = [(Spades, King), (Spades, Queen), (Spades, Nine), (Spades, Seven),
                   (Hearts, King), (Hearts, Jack), (Hearts, Three), (Hearts, Two),
                   (Diamonds, Jack), (Diamonds, Six), (Diamonds, Five), (Diamonds, Three),
                   (Clubs, Ace)]

-- Premptable bid
testHandPrempt_a :: Hand
testHandPrempt_a  = [(Spades, King), (Spades, Queen), (Spades, Nine), (Spades, Seven), (Spades, Six), (Spades, Five), (Spades, Four),
                     (Hearts, King), (Hearts, Jack), (Hearts, Three),
                     (Diamonds, Seven), (Diamonds, Six),
                     (Clubs, Five)]




