{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}
module StockOrder where
import Data.Aeson
import Control.Applicative 
import Venue
import Stock

data OrderDirection = Buy | Sell
data OrderType = Limit | Market | FillOrKill | ImmediateOrCancel

instance Show OrderDirection where
  show Buy = "buy"
  show Sell = "sell"

instance Show OrderType where
  show Limit = "limit"
  show Market = "market"
  show FillOrKill = "fill-or-kill"
  show ImmediateOrCancel = "immediate-or-cancel"

newtype TradingAccount = TradingAccount String
data StockOrderData = StockOrderData {
  tradingAccount :: TradingAccount,
  venueSymbol :: VenueSymbol,
  stock :: Stock,
  price :: Int,
  qty :: Int,
  direction :: OrderDirection,
  orderType :: OrderType
}

instance ToJSON StockOrderData where
  toJSON StockOrderData{..} = 
    object ["account" .= let TradingAccount acc = tradingAccount in acc,
            "venue" .= venueSymbol,
            "stock" .= symbol stock,
            "price" .= price,
            "qty" .= qty,
            "direction" .= show direction,
            "orderType" .= show orderType]
   
data StockOrderConformation = StockOrderConformation {
  
}

instance FromJSON StockOrderConformation where
  parseJSON(Object o) = return StockOrderConformation


