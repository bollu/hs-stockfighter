{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Stockfighter where

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

import Orderbook
import Venue
import Stock
import Heartbeat
import StockOrder

type VenueURL a = "ob" :> "api" :> "venues" :> Capture "venue" VenueSymbol :> a
type StockURL a = VenueURL ("stocks" :> Capture "stock" StockSymbol :> a)

type StockfighterAPI = "ob" :> "api" :> "heartbeat" :> Get '[JSON] Heartbeat -- Heartbeat
                       :<|> VenueURL ("heartbeat" :> Get '[JSON] Venue) -- Venue Heartbeat
                       :<|>  VenueURL ("stocks" :> Get '[JSON] StockList) -- Venue stocks
                       :<|> StockURL (Get '[JSON] Orderbook) -- Stock Orderbook
                       :<|> VenueURL ("stocks" :> Capture "stock" StockSymbol :> "orders" :> ReqBody '[JSON] StockOrderData :> Post '[JSON] StockOrderConformation) -- Place order

stockfighterAPI :: Proxy StockfighterAPI
stockfighterAPI = Proxy

getHeartbeat :: Manager -> BaseUrl -> ExceptT ServantError IO Heartbeat
getVenueHeartbeat :: VenueSymbol -> Manager -> BaseUrl -> ExceptT ServantError IO Venue
getVenueStocksList :: VenueSymbol -> Manager -> BaseUrl -> ExceptT ServantError IO StockList
getStockOrderbook :: VenueSymbol -> StockSymbol -> Manager -> BaseUrl -> ExceptT ServantError IO Orderbook 
placeStockOrder :: VenueSymbol -> StockSymbol -> StockOrderData -> Manager -> BaseUrl -> ExceptT ServantError IO StockOrderConformation 

(getHeartbeat
    :<|> getVenueHeartbeat
    :<|> getVenueStocksList
    :<|> getStockOrderbook 
    :<|> placeStockOrder) = client stockfighterAPI

stockfighterBaseUrl = BaseUrl Https "api.stockfighter.io" 443 ""
