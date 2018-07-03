{-# LANGUAGE OverloadedStrings #-}
-- |
--
module Pymble.Telnet.Api.Parser
  (
  -- *
    Command(..)
  , ParsingError

  -- *
  , parseCommand
  , parseCommandStr
  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.ByteString hiding (pack)
import Data.ByteString.Char8 (pack)
import Text.Trifecta
----------------------------------------------------------------------

-- |
--
data Command =
    Help
  | ViewConfig
  | UpdateConfig
  | Render
  | Quit


-- |
--
type ParsingError = String


-- |
--
parseCommand :: ByteString -> Either ParsingError Command
parseCommand str = undefined


-- |
--
parseCommandStr :: String -> Either ParsingError Command
parseCommandStr = parseCommand . pack