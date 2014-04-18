{-# LANGUAGE OverloadedStrings #-}

module BridgeBuddy.CardsJson where

import BridgeBuddy.Cards

import Data.Aeson (Value(String), toJSON, ToJSON, Object, object)

-- JSON Exports

instance ToJSON Suit where
    toJSON Clubs    = String "clubs"
    toJSON Diamonds = String "diamonds"
    toJSON Hearts   = String "hearts"
    toJSON Spades   = String "spades"

instance ToJSON Rank where
	toJSON          = toJSON . show

instance ToJSON Player where
	toJSON North    = String "N"
	toJSON East     = String "E"
	toJSON West     = String "W"
	toJSON South    = String "S"

instance ToJSON Bid where
	toJSON          = toJSON . show

mkJson :: Hand -> Value
mkJson hand = object [
                ("spades", toJSON $ rankValues Spades hand),
                ("hearts", toJSON $ rankValues Hearts hand),
                ("diamonds", toJSON $ rankValues Diamonds hand),
                ("clubs", toJSON $ rankValues Clubs hand),
                ("hcp", toJSON $ hcp hand),
                ("balanced", toJSON $ isBalanced hand),
                ("bid", toJSON bid ),
                ("reasons", toJSON reasons)
            ]
            where opening_bid = openingBid hand
                  bid = fst opening_bid
                  reasons = snd opening_bid
    
