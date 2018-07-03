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
  , parseCommandBS

  -- *
  , commandParser
  , helpParser

  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.ByteString hiding (unpack)
import Data.ByteString.Char8 (unpack)
import Data.Void


import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
----------------------------------------------------------------------

-- |
--
data Command =
    Help
  | ViewConfig
  | UpdateConfig
  | Render
  | Quit
  deriving (Show)


-- |
--
type ParsingError = String

-- |
--
type Parser = Parsec Void String


-- |
--
parseCommand :: String -> Either ParsingError Command
parseCommand str = case (parse commandParser "" str) of
  Left err -> Left "err" 
  Right xs -> Right xs


-- |
--
parseCommandBS :: ByteString -> Either ParsingError Command
parseCommandBS = parseCommand . unpack


-- |
--
commandParser :: Parser Command
commandParser = helpParser


-- |
--
helpParser :: Parser Command
helpParser = 
  word "help" >> return Help

----------------------------------------------------------------------

-- |
--
word :: String -> Parser ()
word w = do
  space
  string w *> notFollowedBy alphaNumChar
  space