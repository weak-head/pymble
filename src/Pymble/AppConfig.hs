{-# LANGUAGE OverloadedStrings #-}

module Pymble.AppConfig
    (
      AppConfig(..)
    , Port
    ) where

import Data.Default
----------------------------------------------------------------------

-- | TCP port.
-- 
type Port = Int

-- | The complete application configuration.
--
data AppConfig = AppConfig {
    _appDbConnectionInfo :: String
  , _appServerPort       :: Port
  } deriving (Show, Read, Eq)


-- | In case if we dont care, the default application configuration
-- could be used. 
--
instance Default AppConfig where
  def = AppConfig {
        _appDbConnectionInfo = "pymble.db"
      , _appServerPort       = 23
      }