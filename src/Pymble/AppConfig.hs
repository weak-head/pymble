{-# LANGUAGE OverloadedStrings #-}

module Pymble.AppConfig (
  AppConfig(..)
) where

import Data.Default


-- | Application startup configuration
--
data AppConfig = AppConfig {
    _appDbConnectionInfo :: String
  , _appServerPort       :: Int
  } deriving (Show, Read, Eq)


instance Default AppConfig where
  def = AppConfig {
        _appDbConnectionInfo = "pymble.db"
      , _appServerPort       = 23
      }