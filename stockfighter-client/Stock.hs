{-# LANGUAGE OverloadedStrings #-}
module Stock(
  Stock(..),
  StockList(..),
  StockSymbol
)  where
import Data.Aeson
import Control.Applicative

type StockSymbol = String
data Stock = Stock {
  name :: String,
  symbol :: StockSymbol
} deriving(Show)

instance FromJSON Stock where
  parseJSON(Object o) =
    Stock <$> o .: "name"
          <*> o .: "symbol"

data StockList = StockList {
  symbols :: [Stock]
} deriving(Show)

instance FromJSON StockList where
  parseJSON(Object o) =
    StockList <$> o .: "symbols"

