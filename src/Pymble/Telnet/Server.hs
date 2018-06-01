{-# LANGUAGE OverloadedStrings #-}

module Pymble.Telnet.Server (
  startServer
) where

import Pymble.AppConfig


-- | Geven the startup configuration, creates
-- and starts a telnet server on the specified port.
--
-- This is a blocking call and the control would not be
-- returned untill the telnet service is terminated.
--
startServer :: AppConfig -> IO ()
startServer = undefined

