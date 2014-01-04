-- Finite Weighted List
-- 

module FiniteWeightedList where

import qualified Data.Map as Map
import System.Random.Shuffle
import Data.Maybe

data FiniteWeightedList a = 
    FiniteWeightedList {
        weights :: Map.Map String Int,
        counts :: Map.Map String Int,
        items :: [a],
        maxItems :: Int
    }

addItem :: FiniteWeightedList a -> String -> a -> FiniteWeightedList a
addItem fwl key item 
    | total_count == maxItems fwl      = fwl   -- List is completely full, don't add any more
    | current_item_pc >= target_item_pc = fwl   -- List has reached it's percentage quota of 'item' - don't add any more
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

addList :: FiniteWeightedList a -> [a] -> (a -> String) -> FiniteWeightedList a
addList fwl xs f = foldl (\ fwl x -> addItem fwl (f x) x) fwl xs

-- Example 
myList :: FiniteWeightedList String
myList = FiniteWeightedList {
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
