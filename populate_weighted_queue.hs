import System.Environment (getArgs)
import Control.Monad
import Control.Monad.Writer
import Cards
import Data.List
import Network.Beanstalk
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Data.Map as Map
import Control.Applicative
import FiniteWeightedList
import System.Random.Shuffle

handList :: FiniteWeightedList Hand
handList = FiniteWeightedList {
    weights = Map.fromList [
        ("Pass", 10),
        ("1C",   10), 
        ("1D",   10),
        ("1H",   10),
        ("1S",   10),
        ("1NT",  10),
        ("2C",   10),
        ("2NT",  10),
        ("3C",   10),
        ("3D",   10),
        ("3H",   10),
        ("3S",   10)
        ],
    counts = Map.empty,
    items = [],
    maxItems = 1000
}

main :: IO ()
main = do
    args <- getArgs
    bs <- connectBeanstalk "127.0.0.1" "11300"
    let num_decks =
            case length args of 
                0 -> 1
                _ -> read (head args) :: Int

    -- Helper function for mapM. These functions take 1 arg
    -- but we're ignoring it.
    let getHand _ = getBiddableHand

    -- Get the list of biddable hands
    hands <- mapM getHand [1 .. num_decks]

    -- Helper function to get the key for the FiniteWeightedList
    let getBid hand = show $ fst $ openingBid hand

    finalList <- shuffleM $ items $ addList handList hands getBid

    forM_ finalList $ \hand -> do
        let json_string = BSL.unpack $ encode $ mkJson hand
        (_, id) <- putJob bs 1 0 10 (B.pack json_string)
        putStrLn $ "Added Job #" ++ show id ++ ": " ++ json_string



