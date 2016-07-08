{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Orderbook where
import Venue
import Data.Aeson
import Data.Text
import Control.Applicative
import Data.Time (UTCTime)
import GHC.Generics

data Bid = Bid {
  bidprice :: Int,
  bidqty :: Int
} deriving(Show)

instance FromJSON Bid where
  parseJSON(Object o) =
    Bid <$> o .: "price"
          <*> o .: "qty"

data Ask = Ask {
  askprice :: Int,
  askqty :: Int
} deriving(Show)

instance FromJSON Ask where
  parseJSON(Object o) =
    Ask <$> o .: "price"
          <*> o .: "qty"
data Orderbook = Orderbook {
  venueName :: VenueName,
  venueSymbol :: VenueSymbol,
  bids :: [Bid],
  asks :: [Ask],
  timestamp :: UTCTime
} deriving(Generic, Show)

instance FromJSON Orderbook where
  parseJSON(Object o) = 
    Orderbook <$> o .: "venue" 
              <*> o .: "symbol" 
              <*> o .: "bids" 
              <*> o .: "asks" 
              <*> o .: "ts" 

