{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import qualified Data.Aeson as Aeson
import Network.Beanstalk
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Control.Exception as E

main :: IO ()
main = scotty 3000 $ do

    get "/hand" $ do
        bs <- liftIO $ connectBeanstalk "127.0.0.1" "11300"
        _ <- liftIO $ watchTube bs (B.pack "OpeningBids")
        e <- liftIO $ E.tryJust (guard . isTimedOutException) (reserveJobWithTimeout bs 1)

        j <- case e of
                    Right job -> do
                        -- This is not pretty. The JSON is stored in Beanstalkd as a ByteString
                        -- and "decode" expects a lazy bytestring.
                        let json_string = BSL.pack $ B.unpack $ job_body job
                        let oJson = case Aeson.decode json_string :: Maybe Aeson.Object of
                                       Just someObject -> Aeson.Object someObject 
                                       Nothing -> Aeson.Null
                        liftIO $ deleteJob bs (job_id job)
                        return oJson
                    Left _    -> 
                        -- Timeout
                        return Aeson.Null
        json j 

    get "/test" $ html "OK"

