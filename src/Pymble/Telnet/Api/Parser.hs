{-# LANGUAGE OverloadedStrings #-}
-- |
--
module Pymble.Telnet.Api.Parser
  (
  -- * Hi-level parsing API
    RequestForAction(..)
  , Command(..)
  , RenderSettings(..)
  , Url
  , Input
  , ErrorInfo
  , parseAction
  , parseActionBS

  -- * Command parsers
  , Parser
  , commandParser
  , helpParser
  , viewConfigParser
  , updateConfigParser
  , renderParser
  , quitParser

  -- * Command parser helpers
  , renderSettingsParser
  , colorSchemeParser
  , widthParser
  , heightParser
  ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Char (ord)
import Data.String.Utils (strip, endswith)
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, symbol)
import Text.Megaparsec.Perm

import Pymble.PrettyPrint.Terminal (ColorScheme(..))
----------------------------------------------------------------------

-- | Request for action is basically any action that is
-- required or could be taken by the pymble server, telnet client
-- or terminal itself. 
--
data RequestForAction 
  = NoInput                          -- ^ Input is whitespace string or empty
  | CRLF                             -- ^ Carriage return & Line feed
  | PymbleCommand Command            -- ^ Pymble command
  | UnknownCommand Input ErrorInfo   -- ^ The command is unknown or incorrectly formatted
  | TelnetControl [Int]              -- ^ Telnet control sequence
  deriving (Show)

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

-- | Image URL.
type Url = String

-- | User input, unchanged, as-is.
type Input     = String

-- | Detailed error info. Not user-friendly.
type ErrorInfo = String

-- | The parser.
type Parser = Parsec Void String

------------------------------

-- | Parses the pymble telnet command and returns
-- either 'Command' object or the reason of the parsing failure.
--
parseAction :: String -> RequestForAction
parseAction str
    | isCRLF          str = CRLF
    | isNull          str = NoInput
    | isTelnetControl str = TelnetControl (ord <$> str)
    | otherwise =
        case (parse commandParser "" (strip str)) of
          Left err -> UnknownCommand str (show err)
          Right xs -> PymbleCommand xs
  where
    isNull     = null . strip
    withCRLF   = endswith "\r\n"
    isCRLF str = withCRLF str && isNull str
    -- RFC-854, RFC-855, http://mars.netanya.ac.il/~unesco/cdrom/booklet/HTML/NETWORKING/node300.html
    isTelnetControl = ((==) 255) . ord . head


-- | Parses the pymble telnet command that is represented as 'ByteString'
-- and returns either 'Command' object or the reason of the parsing failure.
--
parseActionBS :: ByteString -> RequestForAction
parseActionBS = parseAction . unpack


-- | Parser for the 'Command' api.
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
   
    urlChar = oneOf ("%-+/;?~:.,#&@=[]_*()" :: String)


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