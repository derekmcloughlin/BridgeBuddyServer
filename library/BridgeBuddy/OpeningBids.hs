{-# LANGUAGE PatternGuards #-}

module BridgeBuddy.OpeningBids 
(
      getBiddableHand   
    , openingBid
    , getOpeningResponse
    , openingResponse
    , keepFindingBiddableHand   -- TODO: this probably shouldn't be exported
    , hasStrong2Opening
    , isPremptable
)
where

import Control.Monad.Writer
import Data.List

import BridgeBuddy.Utils
import BridgeBuddy.Cards


isWeak1NTHand :: Hand -> Bool
isWeak1NTHand hand = isBalanced hand && hcp hand `between` (12, 14)

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
    | isBalanced hand && hcp hand `between` (20, 22) = do
        tell ["Balanced hand 20-22 points."]
        return (NT 2)
    | isBalanced hand && hcp hand `between` (15, 19) = do
        tell ["Balanced hand but too many points for 1NT so bid a suit."]
        bidSuit hand
    | (Just suit) <- hasStrong2Opening hand = do
        tell ["Between 16 and 22 points"]
        tell ["At least 8 playing tricks"]
        tell ["Good 6+ card suit or two good 5-4 suits"]
        tell ["Strong two opening"]
        return (Trump suit 2)
    | not (isBalanced hand) && hcp hand `between` (12, 19) = do
        tell ["Unbalanced hand so bid a suit."]
        bidSuit hand
    | ruleOfTwenty hand = do
        tell ["Open using the rule of 20."]
        bidSuit hand
    | hcp hand >= 23 = do
        tell ["23 points or more - artificial 2C bid."]
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
                        hcp hand `between` (16, 22) &&
                        playingTricks hand >= 8
                  strongSuit
                      | hasGoodSixCard (spades hand)    = Just Spades
                      | hasGoodSixCard (hearts hand)    = Just Hearts
                      | hasGoodSixCard (diamonds hand)  = Just Diamonds
                      | otherwise                       = Nothing


-- If you have 10 or 11 HCP and the length of your two
-- longest suits plus your HCP >= 20 then make a bid
ruleOfTwenty :: Hand -> Bool
ruleOfTwenty hand = hcp hand `between` (10, 11) && 
                    (hcp hand + sum [snd q | q <- twoLongestSuits]) >= 20
    where twoLongestSuits = take 2 $ sortBy (flip compareByLength) $ suitLengths hand

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
        innerFind :: Int -> TableHands -> Maybe TableHands
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
        Just tbl -> if hcp (east tbl) <= 6 && hcp (south table) >= 6 then
                        Just tbl
                    else
                        Nothing

        
-- Get the response to an opening bid.
-- bid = North's opening bid. 
-- hand = South's hand
-- Return value is South's response
openingResponse :: Bid -> Hand -> (Bid, [String])
openingResponse bid hand = runWriter $ openingResponseWithLog bid hand

openingResponseWithLog :: Bid -> Hand -> Writer [String] Bid
openingResponseWithLog (NT 1) hand 
    -- Balanced responses
    | isBalanced hand && not (hasGoodFiveCardMajor hand) && hcp hand <= 10 = do
        tell ["Balanced hand but too weak to respond."]
        return Pass
    | isBalanced hand && hcp hand `elem` [11, 12] = do
        tell ["Balanced hand with 11 or 12 hcp"]
        tell ["Respond NT 2 - invitation to game"]
        return (NT 2)
    | isBalanced hand && hcp hand `elem` [13..18] = do
        tell ["Balanced hand and hcp between 13 and 18 - go to game"]
        return (NT 3)
    | isBalanced hand && hcp hand `elem` [19..20] = do
        tell ["Balanced hand and 19 or 20 points"]
        tell ["Respond NT 4 - invitation to 6NT"]
        return (NT 4)
    -- Unbalanced responses
    | hcp hand <= 10 = do
        tell ["Unbalanced hand with <= 10 HCP"]
        respondLongestSuit $ longestSuits hand
    | otherwise = do
        tell ["Unknown config for 1NT"]
        return Pass

openingResponseWithLog _ _ = do
    tell ["Only doing 1NT responses"]
    return Pass

respondLongestSuit :: [(Suit, Int)] -> Writer [String] Bid
respondLongestSuit [(suit, _)] = do
    tell ["Weak take out."]
    tell ["One long suit - bid it."]
    return (Trump suit 2)   -- Weak take out

respondLongestSuit  _ = do
    tell ["Unknown configuration for longest suit response - pass."]
    return Pass

