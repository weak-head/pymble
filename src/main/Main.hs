module Main (main) where

import Data.Default

import Pymble.AppConfig
import Pymble.Telnet.Server (startServer)

-- | Application Main
--
main :: IO ()
main = do
  startServer def

