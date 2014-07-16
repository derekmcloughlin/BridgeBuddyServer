{-# LANGUAGE PatternGuards #-}

module BridgeBuddy.Cards where

import System.Random.Shuffle
import Control.Monad.Writer
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

-- A good 5-card suit is one headed by an honour (=> at least a Jack => hcp > 0)
hasGoodFiveCard :: SuitHolding -> Bool
hasGoodFiveCard sh = cardLength sh == 5 && hcpSuitHolding sh > 0

honours :: SuitHolding -> [Rank]
honours (SuitHolding rs) = filter isHonour rs

-- A good 6-card suit is one headed by two honours
hasGoodSixCard :: SuitHolding -> Bool
hasGoodSixCard suit_holding = cardLength suit_holding >= 6 && length (honours suit_holding) >= 2

isWeak1NTHand :: Hand -> Bool
isWeak1NTHand hand = isBalanced hand && hcp hand `elem` [12 .. 14]

-- Sort a hand by the length of suits. If there are 2 or 3 equal-length suits, sort by Suit descending
longestSuits :: Hand -> [(Suit, Int)]
longestSuits hand = reverse [p | p <- suit_lengths, snd p == snd (head suit_lengths)]
    where suit_lengths = sortBy (flip compareByLength) $ suitLengths hand

-- Rules for bidding suits.
-- This relies on the array being sorted by suit.
-- Need to double check that this is always the case
bidLongestSuit :: [(Suit, Int)] -> Writer [String] Bid
bidLongestSuit [(suit, _)] = do
    tell ["Bid the longest suit."]
    return (Trump suit 1)
bidLongestSuit [(Spades, 4), (Hearts, 4)] = do
    tell ["4 Spades and 4 Hearts - bid the hearts."]
    return (Trump Hearts 1)
bidLongestSuit [(suit, _), (_, _)] = do
    tell ["Bid the higher ranking of two equal-length suits."]
    return (Trump suit 1)
bidLongestSuit [(Spades, 4), (Hearts, 4),   (Diamonds, 4)] = do
    tell ["Shape is 4441 with a singleton club - bid the middle of the 3 touching suits."]
    return (Trump Hearts 1)
bidLongestSuit [(Hearts, 4), (Diamonds, 4), (Clubs, 4)] = do
    tell ["Shape is 4441 with a singleton spade - bid the middle of the 3 touching suits."]
    return (Trump Diamonds 1)
bidLongestSuit [(Spades, 4), (Hearts, 4),   (Clubs, 4)] = do
    tell ["Shape is 4441 with a singleton diamond - bid the suit below."]
    return (Trump Clubs 1)
bidLongestSuit [(Spades, 4), (Diamonds, 4), (Clubs, 4)] = do
    tell ["Shape is 4441 with a singleton heart - bid the suit below."]
    return (Trump Diamonds 1)
bidLongestSuit _ = do
    tell ["Unknown configuration for longest suit bid - pass."]
    return Pass

-- When bidding a suit, bid the longest one.
bidSuit :: Hand -> Writer [String] Bid
bidSuit hand = bidLongestSuit longest_suits
               where longest_suits = longestSuits hand

-- Rules for an opening bid using the writer monad to log
-- decisions made as we go along
openingBidWithLog :: Hand -> Writer [String] Bid
openingBidWithLog hand
    | isWeak1NTHand hand && hasGoodFiveCard (spades hand) = do
        tell ["Balanced hand, but has a five card major in Spades."]
        return (Trump Spades 1)
    | isWeak1NTHand hand && hasGoodFiveCard (hearts hand) = do
        tell ["Balanced hand, but has a five card major in Hearts."]
        return (Trump Hearts 1)
    | isWeak1NTHand hand = do
        tell ["Balanced hand 12-14 points."]
        return (NT 1)
    | isBalanced hand && hcp hand `elem` [20 .. 22] = do
        tell ["Balanced hand 20-22 points."]
        return (NT 2)
    | isBalanced hand && hcp hand `elem` [15 .. 19] = do
        tell ["Balanced hand but too many points for 1NT so bid a suit."]
        bidSuit hand
    | (Just suit) <- hasStrong2Opening hand = do
        tell ["Between 16 and 22 points"]
        tell ["At least 8 playing tricks"]
        tell ["Good 6+ card suit or two good 5-4 suits"]
        tell ["Strong two opening"]
        return (Trump suit 2)
    | not (isBalanced hand) && hcp hand `elem` [12 .. 19] = do
        tell ["Unbalanced hand so bid a suit."]
        bidSuit hand
    | ruleOfTwenty hand = do
        tell ["Open using the rule of 20."]
        bidSuit hand
    | hcp hand >= 23 = do
        tell ["More than 23 points - artificial 2C bid."]
        return (Trump Clubs 2)
    | (True, longestSuit) <- isPremptable hand = do
        tell ["Weak hand with 7-card suit headed by 2 honours."]
        tell ["Opening with a pre-emptive bid at the 3 level."]
        return (Trump longestSuit 3)
    | otherwise = do
        tell ["Too weak to bid."]
        return Pass

-- Weak prempt at the 3-level
--      Hand is weak (< 10 points) - the weaker the better
--      Must have 7+ cards in a suit
--      Must have at least 2 honours in that suit
isPremptable :: Hand -> (Bool, Suit)
isPremptable hand = (hcp hand < 10 && has7cardsuit && has2OfTop3Honours hand longestSuit, longestSuit)
                    where (longestSuit, len) = head $ longestSuits hand
                          has7cardsuit       = len >= 7

has2OfTop3Honours :: Hand -> Suit -> Bool
has2OfTop3Honours hand suit = length (filter (== True) $ map (hasRankInSuit hand suit) [Ace, King, Queen]) >= 2

hasRankInSuit :: Hand -> Suit -> Rank -> Bool
hasRankInSuit hand suit rank = rank `elem` rs
                         where (SuitHolding rs) = suitHolding suit hand

-- Strong 2 Opening
-- 16 - 22 points
-- 8 playing tricks
-- Good 6+ card in Spades, Hearts or Diamonds (2 Clubs is an artificial bid and isn't used here).
-- or good 5-4 suits
hasStrong2Opening :: Hand -> Maybe Suit
hasStrong2Opening hand
                | not isStrongInPoints = Nothing
                | otherwise            = strongSuit
            where isStrongInPoints = not (isBalanced hand) &&
                        hcp hand `elem` [16 .. 22] &&
                        playingTricks hand >= 8
                  strongSuit
                      | hasGoodSixCard (spades hand)    = Just Spades
                      | hasGoodSixCard (hearts hand)    = Just Hearts
                      | hasGoodSixCard (diamonds hand)  = Just Diamonds
                      | otherwise                       = Nothing


-- If you have 10 or 11 HCP and the length of your two
-- longest suits plus your HCP >= 20 then make a bid
ruleOfTwenty :: Hand -> Bool
ruleOfTwenty hand = hcp hand `elem` [10, 11] && 
                    (hcp hand + sum [snd q | q <- twoLongestSuits]) >= 20
    where twoLongestSuits = take 2 $ sortBy (flip compareByLength) $ suitLengths hand

-- Calculate the number of playing tricks in the hand.
-- Note: most systems have the concept of half a playing trick.
-- Here we just round the final result down to the nearest Int.
-- See http://www.rpbridge.net/8j17.htm for the system used here.
-- K, Q-x, K-x, J-10-x, Q-x-x               0.5          A-K, K-Q-J, A-Q-10, A-K-x  2
-- A, K-J, K-Q, A-x, Q-J-x, K-x-x, A-x-x    1            A-Q-J, A-K-J               2.5
-- A-J, A-Q, K-J-10, K-Q-x, A-J-x, A-Q-x    1.5          A-K-Q                      3
playingTricks :: Hand -> Int
playingTricks hand = floor $ sum [playingTricksInSuit $ suitHolding suit hand | suit <- [Clubs .. Spades]]

-- Calculate the playing tricks for cards in a suit
-- We need to turn the suit holding into the form H-H-x where H is an
-- honour (10 is included for this purpose) and 'x' is a non-honour card.
-- We only count max of three cards for the calculation and
-- then add the length if the length is > 3.
playingTricksInSuit :: SuitHolding  -> Double
playingTricksInSuit sh@(SuitHolding rs) = playingTricksInHonours (showHonours sh) + 
                                            fromIntegral lengthTricks
    where lengthTricks = if cardLength (SuitHolding rs) > 3
                         then cardLength (SuitHolding rs) - 3
                         else 0

playingTricksInHonours :: String -> Double
playingTricksInHonours "K"      = 0.5
playingTricksInHonours "Q-x"    = 0.5
playingTricksInHonours "K-x"    = 0.5
playingTricksInHonours "J-10-x" = 0.5
playingTricksInHonours "Q-x-x"  = 0.5

playingTricksInHonours "A"      = 1
playingTricksInHonours "K-J"    = 1
playingTricksInHonours "K-Q"    = 1
playingTricksInHonours "A-x"    = 1
playingTricksInHonours "Q-J-x"  = 1
playingTricksInHonours "K-x-x"  = 1
playingTricksInHonours "A-x-x"  = 1

playingTricksInHonours "A-J"    = 1.5
playingTricksInHonours "A-Q"    = 1.5
playingTricksInHonours "K-J-10" = 1.5
playingTricksInHonours "K-Q-x"  = 1.5
playingTricksInHonours "A-J-x"  = 1.5
playingTricksInHonours "A-Q-x"  = 1.5

playingTricksInHonours "A-K"    = 2
playingTricksInHonours "K-Q-J"  = 2
playingTricksInHonours "A-Q-10" = 2
playingTricksInHonours "A-K-x"  = 2

playingTricksInHonours "A-Q-J"  = 2.5
playingTricksInHonours "A-K-J"  = 2.5

playingTricksInHonours "A-K-Q"  = 3

playingTricksInHonours _        = 0

-- Make an opening bid
openingBid :: Hand -> (Bid, [String])
openingBid hand = runWriter $ openingBidWithLog hand

-- Take a dealt deck and return any hands that have an opening bid
biddableHands :: TableHands -> [Hand]
biddableHands table_hands = filter (\h -> isBiddable h || isNearlyBiddable h ) hands
    where hands = [h table_hands | h <- [north, east, south, west] ]
          bid h = fst (openingBid h)
          isBiddable hand = bid hand /= Pass
          isNearlyBiddable hand = hcp hand == 11 && bid hand == Pass

-- Randomly generate a list of biddable hands
getBiddableHands :: IO [Hand]
getBiddableHands = do
    deck <- shuffleDeck fullDeck
    let hands = biddableHands $ dealHands deck
    case length hands of
            -- Keep trying if we don't get any biddable hands
            0 -> getBiddableHands
            _ -> return hands

-- Get a single biddable hand
getBiddableHand :: IO Hand
getBiddableHand = do
    hands <- getBiddableHands
    return (head hands)

-- Create a string for use in JSON output
-- that shows the ranks of a suit in the form "A K Q 10 5"
rankValues :: Suit -> Hand -> String
rankValues suit hand = unwords [show rank | rank <- rs]
                    where (SuitHolding rs) = suitHolding suit hand

-- Rotate the table anti-clockwise so that 'East' becomes 'North etc.
-- Used to ensure that the first biddable hand is North 
rotate :: TableHands -> TableHands
rotate th = TableHands {
        north = east th,
        east  = south th,
        south = west th,
        west  = north th
    }

-- Go through the hands from North, East, South, West to find the
-- first biddable one, then make that North.
-- This allows us to look for responses where East is weak
-- and South can possibly respond.
keepFindingBiddableHand :: TableHands -> Maybe TableHands
keepFindingBiddableHand table = innerFind 0 table
    where 
        innerFind num_rotations tbl
            | num_rotations == 4 = Nothing  -- Passed out
            | otherwise = do
                let (northsBid, _) = openingBid $ north tbl
                case northsBid of
                    Pass    -> innerFind (num_rotations + 1) $ rotate tbl
                    _       -> Just tbl


-- Find a table where North has an opening bid, East is
-- weak (defined here as <= 6 hcp) and where South has >= 6 hcp
getOpeningResponse :: TableHands -> Maybe TableHands
getOpeningResponse table = 
    case keepFindingBiddableHand table of
        Nothing -> Nothing
        Just tbl -> do
            case hcp (east tbl) <= 6 && hcp (south table) >= 6 of
                True  -> Just tbl
                False -> Nothing

        

