{-# LANGUAGE OverloadedStrings #-}
-- |
--
module Pymble.Telnet.Api.Parser
  (
  -- * Hi-level parsing API
    Command(..)
  , RenderSettings(..)
  , ParsingError
  , parseCommand
  , parseCommandBS

  -- * Command parsers
  , Parser
  , commandParser
  , helpParser
  , viewConfigParser
  , updateConfigParser
  , renderParser
  , quitParser

  -- * Parser helpers
  , renderSettingsParser
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
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, symbol)
import Text.Megaparsec.Perm

import Pymble.PrettyPrint.Terminal (ColorScheme(..))

----------------------------------------------------------------------

-- | This data model defines the command API
-- between a telnet client and the pymble server.
--
data Command =
    Help
  | ViewConfig
  | UpdateConfig RenderSettings
  | Render RenderSettings Url
  | Quit
  deriving (Show, Eq)

-- | The ASCII art renderer settings.
--
data RenderSettings = RenderSettings {
    _csColor  :: Maybe ColorScheme    -- ^ ASCII art color scheme
  , _csWidth  :: Maybe Int            -- ^ ASCII art width (in characters)
  , _csHeight :: Maybe Int            -- ^ ASCII art height (in characters)
  } deriving (Eq, Show)

-- |
--
type Url = String

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
      try helpParser
  <|> try viewConfigParser
  <|> try updateConfigParser
  <|> try renderParser
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
    rs <- renderSettingsParser >>= validate
    eof
    return $ UpdateConfig rs 
  where
    validate (RenderSettings Nothing Nothing Nothing) =
      fail "Explicit configuration settings are expected"
    validate cs = return cs


-- | Parser for the 'Render' command.
--
renderParser :: Parser Command
renderParser = do
    keyword
    rs  <- renderSettingsParser
    url <- uri
    eof
    return $ Render rs url
  where
    keyword = try (word "render")
          <|> try (word "r")


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

-- | 'RenderSettings' parser.
--
renderSettingsParser :: Parser RenderSettings
renderSettingsParser =
  makePermParser (RenderSettings
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

-- | Parses URI.
--
uri :: Parser String
uri = do
    space
    u <- someUrl
    space
    return u
  where
    someUrl = try (wrap "\'" urlStr)
          <|> try (wrap "\"" urlStr)
          <|> try urlStr

    urlStr = some ( try letterChar
                <|> try alphaNumChar
                <|> try urlChar )
   
    urlChar = oneOf ("%-+/;?~:.,#&@=[]" :: String)

-- | Parses wrapped parser.
wrap :: String
     -> Parser a
     -> Parser a
wrap p = between (string p) (string p)


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