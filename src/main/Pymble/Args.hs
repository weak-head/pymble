{-# LANGUAGE OverloadedStrings #-}
-- |
--
module Pymble.Args
    ( 
    -- *
      StartupMode(..)
    , startupMode
    -- *
    , telnetServer
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

import Pymble.AppConfig (Port)
import Pymble.PrettyPrint.Telnet (ColorScheme(..))
----------------------------------------------------------------------

-- |
--
type Width  = Int


-- |
--
type Height = Int


-- |
--
type Url = String


-- |
--
data StartupMode
  = TelnetServer Port
  | DirectConvert (Maybe Width) (Maybe Height) (Maybe ColorScheme) Url


-- |
--
startupMode :: Parser StartupMode
startupMode = directConvert <|> telnetServer


-- |
--
telnetServer :: Parser StartupMode
telnetServer = TelnetServer <$> option auto
    (  long "port"
    <> short 'p'
    <> help "Telnet server port"
    <> showDefault
    <> value 23
    <> metavar "INT" )


-- |
--
directConvert :: Parser StartupMode
directConvert = DirectConvert
 <$> (optional $ option auto
        (  long "width"
        <> short 'w'
        <> help "Width of the generated ASCII art (char)"
        <> metavar "INT"))
 <*> (optional $ option auto
        (  long "height"
        <> short 'h'
        <> help "Height of the generated ASCII art (char)"))
 <*> (optional $ option auto
        (  long "color"
        <> short 'c'
        <> help "Color scheme of the generated ASCII art"))
 <*> (strArgument 
        (  metavar "URL" 
        <> help "URL or filepath of the image to convert"))