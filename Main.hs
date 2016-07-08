{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  result <- runExceptT (getHeartbeat manager stockfighterBaseUrl)
  print $ result
  result <- runExceptT ((getVenueHeartbeat "TESTEX") manager stockfighterBaseUrl)
  print $ result
  result <- runExceptT ((getVenueStocksList "TESTEX") manager stockfighterBaseUrl)
  print $ result
  result <- runExceptT ((getStockOrderbook "TESTEX" "FOOBAR") manager stockfighterBaseUrl)
  print $ result

