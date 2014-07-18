module CardsTest where

import BridgeBuddy.Cards

import Test.Framework                       (Test)
import Test.Framework.Providers.HUnit       (testCase)
import Test.HUnit                           (assertBool, assertEqual)

balancedHand :: Hand
balancedHand  = Hand {
    spades   = SuitHolding [Ace, King, Nine, Seven],
    hearts   = SuitHolding [Queen, Five, Two],
    diamonds = SuitHolding [King, Nine, Five],
    clubs    = SuitHolding [Jack, Six, Three]
}

unbalancedHand :: Hand
unbalancedHand = Hand {
    spades   = SuitHolding [Ace, King, Nine, Seven, Five],
    hearts   = SuitHolding [Queen, Five, Two],
    diamonds = SuitHolding [King],
    clubs    = SuitHolding [Jack, Six, Four, Three]
}

fiveCardMajorHand :: Hand
fiveCardMajorHand  = Hand {
    spades   = SuitHolding [Ace, King, Nine, Seven, Five],
    hearts   = SuitHolding [Queen, Five, Two],
    diamonds = SuitHolding [King, Nine],
    clubs    = SuitHolding [Jack, Six, Three]
}

tests :: [Test]
tests = [     testBalancedHand
            , testUnbalancedHand
            , test5CardMajorSpades
        ]

testBalancedHand :: Test
testBalancedHand  = testCase "testBalancedHand" $ 
    assertBool "Hand should be balanced" (isBalanced balancedHand)

testUnbalancedHand :: Test
testUnbalancedHand = testCase "testUnbalancedHand" $ 
    assertBool "Hand should not be balanced" $ not (isBalanced unbalancedHand)


test5CardMajorSpades :: Test
test5CardMajorSpades = testCase "test5CardMajorSpades" $ 
    assertBool "test5CardMajorSpades" (hasGoodFiveCard (spades fiveCardMajorHand))

testPlayingTricksA :: Test
testPlayingTricksA = testCase "testPlayingTricksA" $ do
    let hand = Hand {
        spades   = SuitHolding [King, Queen, Nine, Seven, Five],
        hearts   = SuitHolding [Queen, Jack, Five, Four, Two],
        diamonds = SuitHolding [Queen],
        clubs    = SuitHolding [Nine, Six]
    }
    assertEqual "Should have 6 playing tricks" 6 (playingTricks hand)
 
