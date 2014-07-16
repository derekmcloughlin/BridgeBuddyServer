module BridgeBuddy.Utils where

between :: Int -> (Int, Int) -> Bool
between n (a, b) = n >= a && n <= b

