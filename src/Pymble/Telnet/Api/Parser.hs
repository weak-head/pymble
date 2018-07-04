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

  -- * Command parsers
  , Parser
  , commandParser
  , helpParser
  , viewConfigParser
  , updateConfigParser
  , quitParser

  -- * Parser combinators
  , configSettingsParser
  , colorSchemeParser
  , widthParser
  , heightParser
  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char (space, string', alphaNumChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Perm

import Pymble.PrettyPrint.Terminal (ColorScheme(..))

----------------------------------------------------------------------

-- |
--
data Command =
    Help
  | ViewConfig
  | UpdateConfig (Maybe ColorScheme) (Maybe Int) (Maybe Int)
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
data ConfigSettings = ConfigSettings {
    _csColor  :: Maybe ColorScheme
  , _csWidth  :: Maybe Int
  , _csHeight :: Maybe Int
  } deriving (Eq, Show)

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
      try helpParser
  <|> try viewConfigParser
  <|> try updateConfigParser
  <|> try quitParser


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


-- | Parser for the 'UpdateConfig' command.
--
updateConfigParser :: Parser Command
updateConfigParser = do
    word "config"
    (ConfigSettings c w h) <- configSettingsParser >>= validate
    eof
    return $ UpdateConfig c w h
  where
    validate (ConfigSettings Nothing Nothing Nothing) =
      fail "Explicit configuration settings are expected"
    validate cs = return cs


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

-- | 'ConfigSettings' parser.
--
configSettingsParser :: Parser ConfigSettings
configSettingsParser =
  makePermParser (ConfigSettings
    <$?> (Nothing, Just <$> colorSchemeParser)
    <|?> (Nothing, Just <$> widthParser)
    <|?> (Nothing, Just <$> heightParser)) 


-- | Parser for the ASCII art 'ColorScheme'.
--
colorSchemeParser :: Parser ColorScheme
colorSchemeParser = do
    keyword >> color
  where
    keyword = try (word "color")
          <|> try (word "c")

    color = try c16
        <|> try c256
        <|> try gs
        <|> try tc
    
    c16  = word "16"  >> return Color16
    c256 = word "256" >> return Xterm256
    gs   = (try (word "gs") <|> try (word "grayscale")) >> return Grayscale
    tc   = (try (word "tc") <|> try (word "truecolor")) >> return TrueColor


-- | Parser for the ASCII art width.
--
widthParser :: Parser Int
widthParser =
    keyword >> number
  where
    keyword = try (word "width")
          <|> try (word "w")


-- | Parser for the ASCII art height.
--
heightParser :: Parser Int
heightParser =
    keyword >> number
  where
    keyword = try (word "height")
          <|> try (word "h")

----------------------------------------------------------------------

-- | Strictly match the word (case insensitive),
-- ignoring whitespace on both sides.
--
word :: String -> Parser ()
word w = do
  space
  string' w *> notFollowedBy alphaNumChar
  space


-- | Parse integral number,
-- ignoring whitespace on both sides.
number :: Integral a => Parser a
number = do
  space
  d <- decimal
  space
  return d