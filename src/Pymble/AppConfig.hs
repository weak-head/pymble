{-# LANGUAGE OverloadedStrings #-}

module Pymble.AppConfig (
    AppConfig(..)
  , Port
) where

import Data.Default

type Port = Int

-- | Application startup configuration
--
data AppConfig = AppConfig {
    _appDbConnectionInfo :: String
  , _appServerPort       :: Port
  } deriving (Show, Read, Eq)


instance Default AppConfig where
  def = AppConfig {
        _appDbConnectionInfo = "pymble.db"
      , _appServerPort       = 23
      }