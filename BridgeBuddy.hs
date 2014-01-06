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
        hand <- getBiddableHand
        printHand "?" hand

printDeck :: IO ()
printDeck = do
    deck <- shuffleDeck
    let hands = dealHands deck
    printHands hands

printHands :: TableHands -> IO ()
printHands hands = do
    printHand "North" $ north hands
    printHand "East" $ east hands
    printHand "South" $ south hands
    printHand "West" $ west hands

printHand :: String -> Hand -> IO ()
printHand position hand = do
    putStrLn position
    putStrLn $ showHand hand
    putStrLn $ "HCP: " ++ show (hcp hand)
    putStrLn $ "Balanced: " ++ show (isBalanced hand)
    let (bid, reasons) = openingBid hand
    putStrLn $ "Bid: " ++ show bid
    putStrLn $ "Reasons: " ++ show reasons

