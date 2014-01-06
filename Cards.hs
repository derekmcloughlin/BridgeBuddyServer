{-# LANGUAGE OverloadedStrings #-}

module Cards where

import System.Random.Shuffle
import Control.Monad.Writer
import Data.List

import Data.Aeson (Value(String), toJSON, ToJSON, Object, object)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq, Ord, Enum)

data Rank = Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two 
            deriving (Eq, Ord, Enum)

data Player = North | East | West | South
              deriving (Show, Eq, Ord, Enum)

data Bid =  Club Int    | 
            Diamond Int |
            Heart Int   |
            Spade Int   |
            NT Int      |
            Pass
            deriving (Eq, Ord)

type Card = (Suit, Rank)
type Hand = [Card]
type Deck = [Card]
type SuitHolding = [Card]
type TableHands = [(Player, Hand)]

-- This is ugly. We're using 'Clubs' to mean the suit, but 'Club' to mean the bid.
makeBid :: Suit -> Int -> Bid
makeBid Clubs n    = Club n
makeBid Diamonds n = Diamond n
makeBid Hearts n   = Heart n
makeBid Spades n   = Spade n

getSuit :: Card -> Suit
getSuit (s, _) = s

getRank :: Card -> Rank
getRank (_, r) = r

fullDeck :: Deck
fullDeck = [(suit, value) | suit <- [Clubs .. Spades], value <- [Ace .. Two]]

shuffleDeck :: IO Deck
shuffleDeck = shuffleM fullDeck

dealHands :: Deck -> TableHands
dealHands deck = [ (North, sort $ take 13 deck),
                   (East,  sort $ take 13 $ drop 13 deck),
                   (West,  sort $ take 13 $ drop 26 deck),
                   (South, sort $ take 13 $ drop 39 deck)]


-- Helper functions for players

north :: TableHands -> Hand
north table_hands = snd $ table_hands !! 0

east :: TableHands -> Hand
east table_hands = snd $ table_hands !! 1

south :: TableHands -> Hand
south table_hands = snd $ table_hands !! 2

west :: TableHands -> Hand
west table_hands = snd $ table_hands !! 3

-- Helper functions for suits

clubs :: Hand -> SuitHolding
clubs = suitHolding Clubs

diamonds :: Hand -> SuitHolding
diamonds = suitHolding Diamonds

hearts :: Hand -> SuitHolding
hearts = suitHolding Hearts

spades :: Hand -> SuitHolding
spades = suitHolding Spades

suitHolding :: Suit -> Hand -> SuitHolding
suitHolding suit hand = sort $ filter (\x -> fst x == suit) hand

-- High Card Point values
hcp :: Hand -> Int
hcp hand = sum $ map hcpValue hand

hcpValue :: Card -> Int
hcpValue (_, Ace)   = 4
hcpValue (_, King)  = 3
hcpValue (_, Queen) = 2
hcpValue (_, Jack)  = 1
hcpValue (_, _)     = 0

showCard :: Card -> String
showCard c = show (getSuit c) ++ " " ++ show (getRank c)

-- Display unicode symbols for suits
instance Show Suit where
   show Clubs = "\9827"
   show Diamonds = "\9830"
   show Hearts = "\9829"
   show Spades = "\9824"

instance Show Rank where
    show Ace = "A"
    show King = "K"
    show Queen = "Q"
    show Jack = "J"
    show Ten = "10"
    show Nine = "9"
    show Eight = "8"
    show Seven = "7"
    show Six = "6"
    show Five = "5"
    show Four = "4"
    show Three = "3"
    show Two = "2"

instance Show Bid where
    show Pass = "Pass"
    show (Club n) = show n ++ "C"
    show (Diamond n) = show n ++ "D"
    show (Heart n) = show n ++ "H"
    show (Spade n) = show n ++ "S"
    show (NT n) = show n ++ "NT"

showHolding :: SuitHolding -> String
showHolding (x:xs) = show (getRank x) ++ " " ++ showHolding xs
showHolding [] = ""

showHand :: Hand -> String
showHand hand = show Spades ++ "  " ++ showHolding (spades hand) ++ "\n" ++
                show Hearts ++ "  " ++ showHolding (hearts hand) ++ "\n" ++
                show Diamonds ++ "  " ++ showHolding (diamonds hand) ++ "\n" ++
                show Clubs ++ "  " ++ showHolding (clubs hand)

-- Get the length of all suits in a hand
suitLengths :: Hand -> [(Suit, Int)]
suitLengths hand = [suitLength suit hand | suit <- [Clubs .. Spades]]

-- Get the length of a suit in a hand
suitLength :: Suit -> Hand -> (Suit, Int)
suitLength  suit hand = (suit, length $ suitHolding suit hand)

-- Helper function to sort a suit by the length
compareByLength :: (Suit, Int) -> (Suit, Int) -> Ordering
compareByLength (_, n1) (_, n2) = compare n1 n2 -- ignore the suits and just look at the length
    
-- A balanced hand has no voids, no singleton and at most one doubleton
-- The shape of the hand can only be one of 5332, 4333 or 4432.
isBalanced :: Hand -> Bool
isBalanced hand = 0 `notElem` suit_lengths &&  -- no voids
                  1 `notElem` suit_lengths &&  -- no singleton
                  (length (filter (== 2) suit_lengths) <= 1) -- one doubleton
                  where suit_lengths = [len | (_, len) <- suitLengths hand]

hasGoodFiveCardMajor :: Hand -> Bool
hasGoodFiveCardMajor hand = hasGoodFiveCard (spades hand) || hasGoodFiveCard (hearts hand)

-- A good 5-card suit is one headed by an honour (=> at least a Jack => hcp > 0)
hasGoodFiveCard :: SuitHolding -> Bool
hasGoodFiveCard cardsInSuit = length cardsInSuit == 5 && hcp cardsInSuit > 0

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
bidLongestSuit [(s, _)] = do
    tell ["Bid the longest suit."]
    return (makeBid s 1)
bidLongestSuit [(Spades, 4), (Hearts, 4)] = do
    tell ["4 Spades and 4 Hearts - bid the hearts."]
    return (Heart 1) 
bidLongestSuit [(s, _), (_, _)] = do
    tell ["Bid the higher ranking of two equal-length suits."]
    return (makeBid s 1)
bidLongestSuit [(Spades, 4), (Hearts, 4),   (Diamonds, 4)] = do
    tell ["Shape is 4441 with a singleton club - bid the middle of the 3 touching suits."]
    return (Heart 1)
bidLongestSuit [(Hearts, 4), (Diamonds, 4), (Clubs, 4)] = do
    tell ["Shape is 4441 with a singleton spade - bid the middle of the 3 touching suits."]
    return (Diamond 1) 
bidLongestSuit [(Spades, 4), (Hearts, 4),   (Clubs, 4)] = do
    tell ["Shape is 4441 with a singleton diamond - bid the suit below."]
    return (Club 1)    
bidLongestSuit [(Spades, 4), (Diamonds, 4), (Clubs, 4)] = do
    tell ["Shape is 4441 with a singleton heart - bid the suit below."]
    return (Diamond 1)
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
        return (Spade 1)
    | isWeak1NTHand hand && hasGoodFiveCard (hearts hand) = do
        tell ["Balanced hand, but has a five card major in Hearts."]
        return (Heart 1)
    | isWeak1NTHand hand = do
        tell ["Balanced hand 12-14 points."]
        return (NT 1)
    | isBalanced hand && hcp hand `elem` [20 .. 22] = do
        tell ["Balanced hand 20-22 points."]
        return (NT 2)
    | isBalanced hand && hcp hand `elem` [15 .. 19] = do 
        tell ["Balanced hand but too many points for 1NT so bid a suit."]
        bidSuit hand
    | not (isBalanced hand) && hcp hand `elem` [12 .. 19] = do
        tell ["Unbalanced hand so bid a suit."]
        bidSuit hand
    | ruleOfTwenty hand = do
        tell ["Open using the rule of 20."]
        bidSuit hand
    | hcp hand >= 23 = do
        tell ["More than 23 points - artificial 2C bid."]
        return (Club 2)
    | fst (isPremptable hand) = do
        let (_, longestSuit) = isPremptable hand
        tell ["Weak hand with 7-card suit headed by 2 honours."]
        tell ["Opening with a pre-emptive bid at the 3 level."]
        return (makeBid longestSuit 3)
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
hasRankInSuit hand suit rank = (suit, rank) `elem` cardsInSuit
                               where cardsInSuit = suitHolding suit hand

-- If you have 10 or 11 HCP and the length of your two
-- longest suits plus your HCP >= 20 then make a bid
ruleOfTwenty :: Hand -> Bool
ruleOfTwenty hand = hcp hand `elem` [10, 11] && (hcp hand + sum [snd q | q <- twoLongestSuits]) >= 20
    where twoLongestSuits = take 2 $ sortBy (flip compareByLength) $ suitLengths hand

-- Make an opening bid
openingBid :: Hand -> (Bid, [String])
openingBid hand = runWriter $ openingBidWithLog hand

-- Take a dealt deck and return any hands that have an opening bid
biddableHands :: TableHands -> [Hand]
biddableHands table_hands = filter (\h -> isBiddable h || isNearlyBiddable h ) hands
    where hands = [h | (_, h) <- table_hands]
          bid h = fst (openingBid h)
          isBiddable hand = bid hand /= Pass
          isNearlyBiddable hand = hcp hand == 11 && bid hand == Pass

-- Randomly generate a list of biddable hands
getBiddableHands :: IO [Hand]
getBiddableHands = do
    deck <- shuffleDeck
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
rankValues s hand = unwords [show n | (_, n) <- suitHolding s hand]

-- JSON Exports

instance ToJSON Suit where
    toJSON Clubs    = String "clubs"
    toJSON Diamonds = String "diamonds"
    toJSON Hearts   = String "hearts"
    toJSON Spades   = String "spades"

instance ToJSON Rank where
	toJSON          = toJSON . show

instance ToJSON Player where
	toJSON North    = String "N"
	toJSON East     = String "E"
	toJSON West     = String "W"
	toJSON South    = String "S"

instance ToJSON Bid where
	toJSON          = toJSON . show

mkJson :: Hand -> Value
mkJson hand = object [
                ("spades", toJSON $ rankValues Spades hand),
                ("hearts", toJSON $ rankValues Hearts hand),
                ("diamonds", toJSON $ rankValues Diamonds hand),
                ("clubs", toJSON $ rankValues Clubs hand),
                ("hcp", toJSON $ hcp hand),
                ("balanced", toJSON $ isBalanced hand),
                ("bid", toJSON bid ),
                ("reasons", toJSON reasons)
            ]
            where opening_bid = openingBid hand
                  bid = fst opening_bid
                  reasons = snd opening_bid
    
