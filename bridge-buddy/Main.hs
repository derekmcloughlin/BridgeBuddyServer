import System.Environment (getArgs)
import Control.Monad

import BridgeBuddy.Cards
import BridgeBuddy.OpeningBids
import BridgeBuddy.OpeningResponses

main :: IO ()
main = do
    args <- getArgs
    let num_decks =
            case args of
                [] -> 1
                (x:_) -> (read x) :: Int

    forM_ [1 .. num_decks] $ \_ -> do
        printDeck

printDeck :: IO ()
printDeck = do
    deck <- shuffleDeck fullDeck
    let table = dealHands deck
    case keepFindingBiddableHand table of
        Just tbl -> do
            case getOpeningResponse tbl of
                Just _ -> do
                    printTable "HAS OPENING RESPONSE" tbl
                Nothing -> 
                    printTable "NO OPENING RESPONSE" tbl
        Nothing  -> do 
            printTable "TABLE PASSED OUT" table

printTable :: String -> TableHands -> IO ()
printTable header hands = do
    putStrLn $ "-------" ++ header
    mapM_ (\(s, f) -> printHand s (f hands)) players
    where players = [ ("North", north),
                      ("East", east),
                      ("South", south),
                      ("West", west)]

printHand :: String -> Hand -> IO ()
printHand position hand = do
    putStrLn position
    putStr   $ show hand
    putStrLn $ "HCP: " ++ show (hcp hand)
    putStrLn $ "Playing Tricks: " ++ show (playingTricks hand)
    putStrLn $ "Balanced: " ++ show (isBalanced hand)
    let (bid, reasons) = openingBid hand
    putStrLn $ "Bid: " ++ show bid
    putStrLn $ "Reasons: " ++ show reasons

