{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Stockfighter(Heartbeat(..),
                   SfCtx,
                   makeSfCtx,
                   Stock(..),
                   OrderDirection(..),
                   OrderType(..),
                   TradingAccount(..),
                   getAPIHeartbeat,
                   getVenueHeartbeat, 
                   getStockList,
                   getOrderbook,
                   placeStockOrder) where
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Time (UTCTime)
--import Control.Monad.Trans.Either
import Control.Monad.Trans.Except(ExceptT, runExceptT)
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Client
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS(tlsManagerSettings)
import Control.Lens.TH
import Control.Lens hiding ((.=))
import Data.Aeson.Lens
import qualified Data.Text as T

--import Orderbook
--import Venue
-- import Stock
--import Heartbeat
--import StockOrder

type ApiKey = Text
newtype TradingAccount = TradingAccount String deriving(Show)

type VenueName = String
type VenueSymbol = String

data SfCtx = SfCtx {
  _sfCtxTradingAccount :: TradingAccount,
  _sfCtxApiKey :: ApiKey,
  _sfCtxVenueSymbol :: VenueSymbol,
  _sfCtxServantManager :: Manager
}

$(makeFields ''SfCtx)

makeSfCtx :: MonadIO m => TradingAccount -> ApiKey -> VenueSymbol -> m SfCtx
makeSfCtx tradingAccount apiKey venueSymbol = liftIO $ do
  manager <- newManager tlsManagerSettings
  return $ SfCtx tradingAccount apiKey venueSymbol manager



data Heartbeat = Heartbeat {
  _heartbeatOk :: Bool,
  _heartbeatErrorString :: String
} deriving(Show)

$(makeFields ''Heartbeat)

instance FromJSON Heartbeat where
  parseJSON (Object o) =
    Heartbeat <$> o .: "ok"
              <*> o .: "error"


type StockSymbol = String
data Stock = Stock {
  _stockName :: String,
  _stockSymbol :: StockSymbol
} deriving(Show)

$(makeFields ''Stock)

instance FromJSON Stock where
  parseJSON(Object o) =
    Stock <$> o .: "name"
          <*> o .: "symbol"

-- we need this because servant returns an [stock] inside what's called
-- symbols. We use this internally but we don't show it to the user
data StockList = StockList {
  _stockListStocks :: [Stock]
} deriving(Show)
$(makeFields ''StockList)

instance FromJSON StockList where
  parseJSON(Object o) =
    StockList <$> o .: "symbols"



data Venue = Venue {
  _venueVenueName :: String
} deriving(Show)

$(makeFields ''Venue)

instance FromJSON Venue where
  parseJSON(Object o) =
    Venue <$> o .: "venue"

data Bid = Bid {
  _bidPrice :: Int,
  _bidQty :: Int
} deriving(Show)

$(makeFields ''Bid)
instance FromJSON Bid where
  parseJSON(Object o) =
    Bid <$> o .: "price"
          <*> o .: "qty"

data Ask = Ask {
  _askPrice :: Int,
  _askQty :: Int
} deriving(Show)

$(makeFields ''Ask)
instance FromJSON Ask where
  parseJSON(Object o) =
    Ask <$> o .: "price"
          <*> o .: "qty"
data Orderbook = Orderbook {
  _orderbookVenueName :: VenueName,
  _orderbookVenueSymbol :: VenueSymbol,
  _orderbookBids ::  [Bid],
  _orderbookAsks ::  [Ask],
  _orderbookTimestamp :: UTCTime
} deriving(Show)

$(makeFields ''Orderbook)


instance FromJSON Orderbook where
  parseJSON(Object o) = 
    Orderbook <$> o .: "venue" 
              <*> o .: "symbol" 
              <*> o .:? "bids" .!= []
              <*> o .:? "asks".!= []
              <*> o .: "ts" 

data OrderDirection = Buy | Sell
$(makeFields ''OrderDirection)
data OrderType = Limit | Market | FillOrKill | ImmediateOrCancel
$(makeFields ''OrderType)

instance Show OrderDirection where
  show Buy = "buy"
  show Sell = "sell"

instance ToJSON OrderDirection where
  toJSON dir = String . T.pack . show $ dir


instance Show OrderType where
  show Limit = "limit"
  show Market = "market"
  show FillOrKill = "fill-or-kill"
  show ImmediateOrCancel = "immediate-or-cancel"

instance ToJSON OrderType where
  toJSON order = String . T.pack . show $ order

instance ToJSON TradingAccount where
  toJSON (TradingAccount(acc)) = String $ T.pack acc

type OrderPrice = Int
type OrderQuantity = Int
data OrderData = OrderData {
  _orderDataTradingAccount :: TradingAccount,
  _orderDataVenueSymbol :: VenueSymbol,
  _orderDataStockSymbol :: StockSymbol,
  _orderDataPrice :: OrderPrice,
  _orderDataQty :: OrderQuantity,
  _orderDataDirection :: OrderDirection,
  _orderDataOrderType :: OrderType
} deriving(Show)

$(makeFields ''OrderData)

instance ToJSON OrderData where
  toJSON o = object $ 
    ["account" .= (o ^. tradingAccount),
     "venue" .= (o ^. venueSymbol),
     "stock" .= (o ^. stockSymbol),
     "price" .= (o ^. price),
     "qty" .= (o ^. qty),
     "direction" .= (o ^. direction),
     "orderType" .= (o ^. orderType)]
   
data Fill = Fill {
  fillPrice :: Int,
  fillQty :: Int,
 fillTimestamp :: UTCTime
} deriving(Show)

instance FromJSON Fill where
  parseJSON(Object o) =
    Fill <$> o .: "price"
         <*> o .: "qty"
         <*> o .: "ts"

data OrderConformation = OrderConformation {
  _orderConformationOriginalQty :: Int,
  _orderConformationOutstandingQty :: Int,
  _orderConformationId :: Int,
  _orderConformationTimestamp :: UTCTime,
  _orderConformationFills :: [Fill],
  _orderConformationTotalFilled :: Int,
  _orderConformationOpen :: Bool
} deriving(Show)

$(makeFields ''OrderConformation)

instance FromJSON OrderConformation where
  parseJSON(Object o) = 
    OrderConformation <$> o .: "originalQty"
                <*> o .: "qty"
                <*> o .: "id"
                <*> o .: "ts"
                <*> o .: "fills"
                <*> o .: "totalFilled"
                <*> o .: "open"



type VenueURL a = "ob" :> "api" :> "venues" :> Capture "venue" VenueSymbol :> a
type StockURL a = VenueURL ("stocks" :> Capture "stock" StockSymbol :> a)

type StockfighterAPI = "ob" :> "api" :> "heartbeat" :> Get '[JSON] Heartbeat -- Heartbeat
                       :<|> VenueURL ("heartbeat" :> Get '[JSON] Venue) -- Venue Heartbeat
                       :<|>  VenueURL ("stocks" :> Get '[JSON] StockList) -- Venue stocks
                       :<|> StockURL (Get '[JSON] Orderbook) -- Stock Orderbook
                       :<|> VenueURL ("stocks" :> Capture "stock" StockSymbol :> "orders" :> ReqBody '[JSON] OrderData :> Header "X-Starfighter-Authorization" ApiKey :> Post '[JSON] OrderConformation) -- Place order

stockfighterAPI :: Proxy StockfighterAPI
stockfighterAPI = Proxy

_getHeartbeat :: Manager -> BaseUrl -> ExceptT ServantError IO Heartbeat
_getVenueHeartbeat :: VenueSymbol -> Manager -> BaseUrl -> ExceptT ServantError IO Venue
_getStockList :: VenueSymbol -> Manager -> BaseUrl -> ExceptT ServantError IO StockList
_getOrderbook :: VenueSymbol -> StockSymbol -> Manager -> BaseUrl -> ExceptT ServantError IO Orderbook 
_placeStockOrder :: VenueSymbol -> StockSymbol -> OrderData -> Maybe ApiKey -> Manager -> BaseUrl -> ExceptT ServantError IO OrderConformation 

sfBaseUrl = BaseUrl Https "api.stockfighter.io" 443 ""

getAPIHeartbeat :: SfCtx -> ExceptT ServantError IO Heartbeat
getAPIHeartbeat sfctx = _getHeartbeat (sfctx ^. servantManager) sfBaseUrl

getVenueHeartbeat :: SfCtx -> ExceptT ServantError IO Venue  
getVenueHeartbeat sfctx = _getVenueHeartbeat (sfctx ^. venueSymbol) 
                          (sfctx ^. servantManager)
                          sfBaseUrl

getStockList :: SfCtx ->  ExceptT ServantError IO [Stock]
getStockList sfctx = do
  stockslist <-  _getStockList (sfctx ^. venueSymbol) 
                          (sfctx ^. servantManager)
                          sfBaseUrl
  return $ stockslist ^. stocks

getOrderbook :: SfCtx -> StockSymbol -> ExceptT ServantError IO Orderbook 
getOrderbook sfctx stocksym = _getOrderbook (sfctx ^. venueSymbol) 
                          stocksym
                          (sfctx ^. servantManager)
                          sfBaseUrl

placeStockOrder :: SfCtx -> StockSymbol -> OrderPrice -> OrderQuantity -> OrderDirection -> OrderType -> ExceptT ServantError IO OrderConformation 
placeStockOrder sfctx stocksym price qty direction otype = 
  _placeStockOrder (sfctx ^. venueSymbol) 
    stocksym
    orderdata 
    (Just $ sfctx ^. apiKey)
    (sfctx ^. servantManager)
    sfBaseUrl
  where
    orderdata = OrderData {
      _orderDataTradingAccount = sfctx ^. tradingAccount,
      _orderDataVenueSymbol = sfctx ^. venueSymbol,
      _orderDataStockSymbol = stocksym,
      _orderDataPrice = price,
      _orderDataQty = qty,
      _orderDataDirection = direction,
      _orderDataOrderType = otype
    }


(_getHeartbeat
    :<|> _getVenueHeartbeat
    :<|> _getStockList
    :<|> _getOrderbook 
    :<|> _placeStockOrder) = client stockfighterAPI

