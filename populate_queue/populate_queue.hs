import System.Environment (getArgs)
import Control.Monad
import Control.Monad.Writer
import Cards
import CardsJson
import Data.List
import Network.Beanstalk
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Data.Map as Map
import Control.Applicative
import FiniteWeightedList
import System.Random.Shuffle

handList :: FiniteWeightedList String Hand
handList = FiniteWeightedList {
    weights = Map.fromList [
        ("Pass", 7),
        ("1C",   7), 
        ("1D",   7),
        ("1H",   7),
        ("1S",   7),
        ("1NT",  7),
        ("2C",   7),
        ("2D",   7),
        ("2H",   7),
        ("2S",   7),
        ("2NT",  7),
        ("3C",   7),
        ("3D",   7),
        ("3H",   7),
        ("3S",   7)
        ],
    counts = Map.empty,
    items = [],
    maxItems = 1000
}

main :: IO ()
main = do
    args <- getArgs
    bs <- connectBeanstalk "127.0.0.1" "11300"
    useTube bs (B.pack "OpeningBids")
    let num_decks =
            case args of
                [] -> 1
                (x:_) -> (read x) :: Int

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



