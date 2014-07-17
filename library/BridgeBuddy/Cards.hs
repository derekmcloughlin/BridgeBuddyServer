module BridgeBuddy.Cards 
(
      Suit (..)
    , Rank (..)
    , Player (..)
    , Bid (..)
    , Hand (..)
    , SuitHolding (..)
    , TableHands (..)
    , hcp
    , shuffleDeck
    , dealHands
    , fullDeck
    , numHonours
    , isHonour
    , showHonour
    , showHonours
    , isBalanced
    , hasGoodFiveCardMajor
    , hasGoodFiveCard
    , hasGoodSixCard
    , suitLengths
    , cardLength
    , suitHolding
    , compareByLength 
)
where

import System.Random.Shuffle
import Data.List
import Data.List.Split

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq, Ord, Enum)

-- Display single-letter codes for suits
instance Show Suit where
   show Clubs    = "C"
   show Diamonds = "D"
   show Hearts   = "H"
   show Spades   = "S"


data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
            deriving (Eq, Ord, Enum)

instance Show Rank where
    show Ace   = "A"
    show King  = "K"
    show Queen = "Q"
    show Jack  = "J"
    show Ten   = "10"
    show Nine  = "9"
    show Eight = "8"
    show Seven = "7"
    show Six   = "6"
    show Five  = "5"
    show Four  = "4"
    show Three = "3"
    show Two   = "2"


data Player = North | East | South | West
              deriving (Show, Eq, Ord, Enum)


data Bid = Trump Suit Int | NT Int | Pass | Dbl | ReDbl
           deriving (Eq, Ord)

instance Show Bid where
    show Pass           = "Pass"
    show (Trump suit n) = show n ++ show suit
    show (NT n)         = show n ++ "NT"
    show (Dbl)          = "Dbl"
    show (ReDbl)        = "ReDbl"


data Card = Card Suit Rank
            deriving (Show, Eq, Ord)

newtype SuitHolding = SuitHolding [Rank]

instance Show SuitHolding where
    show (SuitHolding cards) = show cards


data Hand = Hand {
        clubs    :: SuitHolding,
        diamonds :: SuitHolding,
        hearts   :: SuitHolding,
        spades   :: SuitHolding
    }

instance Show Hand where
    show hand = show Spades ++ "  " ++ show (spades hand) ++ "\n" ++
                show Hearts ++ "  " ++ show (hearts hand) ++ "\n" ++
                show Diamonds ++ "  " ++ show (diamonds hand) ++ "\n" ++
                show Clubs ++ "  " ++ show (clubs hand) ++ "\n" 

newtype Deck = Deck [Card]
               deriving (Show)

data TableHands = TableHands {
        north :: Hand,
        east  :: Hand,
        south :: Hand,
        west  :: Hand
    }
    deriving (Show)

suitHolding :: Suit -> Hand -> SuitHolding
suitHolding Clubs hand    = clubs hand
suitHolding Diamonds hand = diamonds hand
suitHolding Hearts hand   = hearts hand
suitHolding Spades hand   = spades hand

-- High Card Point values
hcp :: Hand -> Int
hcp hand = sum $ map hcpSuitHolding [suit hand | suit <- [clubs, diamonds, hearts, spades]]

hcpSuitHolding :: SuitHolding -> Int
hcpSuitHolding (SuitHolding rs) = sum $ map hcpValue rs

hcpValue :: Rank -> Int
hcpValue Ace   = 4
hcpValue King  = 3
hcpValue Queen = 2
hcpValue Jack  = 1
hcpValue _     = 0

cardLength :: SuitHolding -> Int
cardLength (SuitHolding rs) = length rs

fullDeck :: Deck
fullDeck = Deck [Card suit rank | suit <- [Clubs .. Spades], rank <- [Two .. Ace]]

shuffleDeck :: Deck -> IO Deck
shuffleDeck (Deck d) = do
    shuffled <- shuffleM d
    return $ Deck shuffled

dealHands :: Deck -> TableHands
dealHands (Deck d) = TableHands {
                        north = mkHand $ hands !! 0,
                        east  = mkHand $ hands !! 1,
                        west  = mkHand $ hands !! 2,
                        south = mkHand $ hands !! 3
                     }
                     where hands = map sort $ chunksOf 13 d
                           mkHand :: [Card] -> Hand
                           mkHand cards = Hand {
                                            clubs    = SuitHolding $ ranks Clubs cards,
                                            diamonds = SuitHolding $ ranks Diamonds cards,
                                            hearts   = SuitHolding $ ranks Hearts cards,
                                            spades   = SuitHolding $ ranks Spades cards
                                        }
                           ranks suit cs = sortBy (flip compare) [r | (Card s r) <- cs, suit == s]

-- A Card is an 'Honour' if it is Ace, King, Queen or Jack.
isHonour :: Rank -> Bool
isHonour Ace    = True
isHonour King   = True
isHonour Queen  = True
isHonour Jack   = True
isHonour _      = False

-- When looking at playing cards, non-honour cards are displayed as x o
showHonour :: Rank -> String
showHonour rank
    | isHonour rank = show rank
    | otherwise     = "x"

-- When looking at playing card strength, cards are shown as e.g. "K-Q-x-x-x" (sometimes just KQxxx)
showHonours :: SuitHolding -> String
showHonours (SuitHolding rs) = intercalate "-" [showHonour n | n <- take 3 $ sortBy (flip compare) rs]


suitLengths :: Hand -> [(Suit, Int)]
suitLengths hand = [(suit, suitLength suit hand) | suit <- [Clubs .. Spades]]

-- Get the length of a suit in a hand
suitLength :: Suit -> Hand -> Int
suitLength suit hand = cardLength $ suitHolding suit hand

-- A balanced hand has no voids, no singleton and at most one doubleton
-- The shape of the hand can only be one of 5332, 4333 or 4432.
isBalanced :: Hand -> Bool
isBalanced hand = 0 `notElem` suit_lengths &&  -- no voids
                  1 `notElem` suit_lengths &&  -- no singleton
                  (length (filter (== 2) suit_lengths) <= 1) -- one doubleton
                  where suit_lengths = [len | (_, len) <- suitLengths hand]

-- Helper function to sort a suit by the length
compareByLength :: (Suit, Int) -> (Suit, Int) -> Ordering
compareByLength (_, n1) (_, n2) = compare n1 n2 -- ignore the suits and just look at the length

hasGoodFiveCardMajor :: Hand -> Bool
hasGoodFiveCardMajor hand = hasGoodFiveCard (spades hand) || hasGoodFiveCard (hearts hand)

-- A good 5-card suit is one headed by an honour
hasGoodFiveCard :: SuitHolding -> Bool
hasGoodFiveCard sh = cardLength sh == 5 && numHonours sh >= 1

honours :: SuitHolding -> [Rank]
honours (SuitHolding rs) = filter isHonour rs

numHonours:: SuitHolding -> Int
numHonours sh = length (honours sh)

-- A good 6-card suit is one headed by two honours
hasGoodSixCard :: SuitHolding -> Bool
hasGoodSixCard suit_holding = cardLength suit_holding >= 6 && numHonours suit_holding >= 2

