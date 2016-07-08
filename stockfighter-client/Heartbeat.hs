{-# LANGUAGE OverloadedStrings #-}
module Heartbeat where
import Data.Aeson
import Control.Applicative 

data Heartbeat = Heartbeat {
  ok :: Bool,
  error :: String
} deriving(Show)

instance FromJSON Heartbeat where
  parseJSON (Object o) =
    Heartbeat <$> o .: "ok"
              <*> o .: "error"

