{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields  #-}

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

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Stockfighter

apikey_acc = "97ef11d3dc636c4e54142ac3f29c3cf8815bdcec"
apikey = "98b71aa87e3bbf50016e97d7bb0117872dfc4258"

account = TradingAccount "HAH20152510"
venue = "YXHPEX"
stock = "HDS"

main :: IO ()
main = do
  sfctx <- makeSfCtx  account apikey venue
  result <- runExceptT $ getAPIHeartbeat sfctx
  print $ result

  result <-  runExceptT $ getVenueHeartbeat sfctx
  print $ result

  result <- runExceptT $  getStockList sfctx
  print $ result

  result <- runExceptT $  getOrderbook sfctx stock
  print $ result

  result <- runExceptT $  placeStockOrder sfctx stock 5142 1 Buy Market
  print $ result
