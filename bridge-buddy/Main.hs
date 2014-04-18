import System.Environment (getArgs)
import Control.Monad
import Control.Monad.Writer
import Cards
import Data.List

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
    let hands = dealHands deck
    printHands hands

printHands :: TableHands -> IO ()
printHands hands = mapM_ (\(s, f) -> printHand s (f hands)) players
    where players = [ ("North", north),
                      ("East", east),
                      ("South", south),
                      ("West", west)]

printHand :: String -> Hand -> IO ()
printHand position hand = do
    putStrLn position
    putStrLn $ show hand
    putStrLn $ "HCP: " ++ show (hcp hand)
    putStrLn $ "Playing Tricks: " ++ show (playingTricks hand)
    putStrLn $ "Balanced: " ++ show (isBalanced hand)
    let (bid, reasons) = openingBid hand
    putStrLn $ "Bid: " ++ show bid
    putStrLn $ "Reasons: " ++ show reasons

