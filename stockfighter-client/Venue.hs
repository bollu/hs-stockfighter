{-# LANGUAGE OverloadedStrings #-}

module Venue where

import Data.Aeson
import Control.Applicative 

type VenueName = String
type VenueSymbol = String

data Venue = Venue {
  venue :: String
} deriving(Show)

instance FromJSON Venue where
  parseJSON(Object o) =
    Venue <$> o .: "venue"
