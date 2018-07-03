{-# LANGUAGE OverloadedStrings #-}
-- |
--
module Pymble.Telnet.Api.Parser
  (
  -- * Hi-level parsing API
    Command(..)
  , ParsingError
  , parseCommand
  , parseCommandBS

  -- * Specific command parsers
  , Parser
  , commandParser
  , helpParser
  , viewConfigParser
  , quitParser
  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char (eol, space, string', alphaNumChar)
----------------------------------------------------------------------

-- |
--
data Command =
    Help
  | ViewConfig
  | UpdateConfig
  | Render
  | Quit
  deriving (Show, Eq)

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
  Left err -> Left "failed to parse" 
  Right xs -> Right xs


-- |
--
parseCommandBS :: ByteString -> Either ParsingError Command
parseCommandBS = parseCommand . unpack


-- |
--
commandParser :: Parser Command
commandParser =
      helpParser
  <|> quitParser


-- | Parser for the 'Help' command.
--
helpParser :: Parser Command
helpParser = 
  word "help" >> return Help


-- | Parser for the 'ViewConfig' command.
--
viewConfigParser :: Parser Command
viewConfigParser =
  word "config" >> eof >> return ViewConfig


-- | Parser for the 'Quit' command.
--
quitParser :: Parser Command
quitParser = 
    keyword >> eof >> return Quit
  where
    keyword = try (word "quit")
          <|> try (word "exit")
          <|> try (word "q")

----------------------------------------------------------------------

-- | Strictly match the word (case insensitive),
-- ignoring whitespace on both sides.
--
word :: String -> Parser ()
word w = do
  space
  string' w *> notFollowedBy alphaNumChar
  space