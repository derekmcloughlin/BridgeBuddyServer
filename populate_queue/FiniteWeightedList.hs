{-
Finite Weighted List
This data structure stores at most N items grouped by a string, with weighted
distribution of each group.

For example: if we want to store a bridge hand and its bid but ensure that we have
a certain distribution of 'rare' bids (e.g. 3C), then we could use this:

    bridgeHands :: FiniteWeightedList Hand
    bridgeHands  = FiniteWeightedList {
        weights = Map.fromList [
            ("1C",   9), 
            ("1D",   9),
            ("1H",   9),
            ("1S",   9),
            ("1NT",  9),
            ("2NT",  9),
            ("2C",   9),
            ("3C",   9),
            ("3D",   9),
            ("3H",   9),
            ("3S",   9),
            ("PASS", 9)
            ],
        counts = Map.empty,
        items = [],
        maxItems = 1000
    }

When populating the list the commonly-occurring bids (1C - 1S) will fill up quickly. Once they've
reached their quota, the rarer bids will fill the rest of the list. Although the list will have
the correct distribution, it won't be random, so you should shuffle the result afterwards if desired.

-}

module FiniteWeightedList where

import qualified Data.Map as Map
import Data.Maybe

data FiniteWeightedList c a = 
    FiniteWeightedList {
        weights :: Map.Map c Int,
        counts :: Map.Map c Int,
        items :: [a],
        maxItems :: Int
    }

-- Add a single item to the list
addItem :: (Ord c) => FiniteWeightedList c a -> c -> a -> FiniteWeightedList c a
addItem fwl key item 
    | total_count == maxItems fwl      = fwl   -- List is completely full, don't add any more
    | current_item_pc >= target_item_pc = fwl  -- List has reached it's percentage quota of 'item' - don't add any more
    | otherwise = FiniteWeightedList {
            weights = weights fwl,
            counts = Map.insert key (num_items + 1) (counts fwl),
            items = items fwl ++ [item],
            maxItems = maxItems fwl
      }
      where total_count             = Map.foldl (+) 0 (counts fwl)
            num_items               = get_map_item key (counts fwl)
            target_item_pc          = get_map_item key (weights fwl) 
            current_item_pc  
                | total_count == 0  = 0
                | otherwise         = (100 * num_items) `div` total_count
            get_map_item k m        = fromMaybe 0 (Map.lookup k m)

-- Add a list of items to the data structure.
-- A function 'f' that gives us the group string is provided.
addList :: (Ord c) => FiniteWeightedList c a -> [a] -> (a -> c) -> FiniteWeightedList c a
addList fwl xs f = foldl (\ tmp_fwl x -> addItem tmp_fwl (f x) x) fwl xs


