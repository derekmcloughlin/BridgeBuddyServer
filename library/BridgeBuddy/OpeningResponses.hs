module BridgeBuddy.OpeningResponses 
(
      getOpeningResponse
    , openingResponse
    , keepFindingBiddableHand   -- TODO: this probably shouldn't be exported
)
where

import Control.Monad.Writer

import BridgeBuddy.Cards
import BridgeBuddy.OpeningBids


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
    | hcp hand < 10 = do
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

