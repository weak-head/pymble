{-# LANGUAGE OverloadedStrings #-}
-- | This module covers the command line argument parsing.
--
module Pymble.Args
    ( 
    -- * Command line argument parsing
      StartupMode(..)
    , Width
    , Height
    , Url
    , startupMode
    -- * Concrete parsers
    , telnetServer
    , directConvert
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

import Pymble.AppConfig (Port)
import Pymble.PrettyPrint.Terminal (ColorScheme(..))
----------------------------------------------------------------------

-- | ASCII art width in characters
type Width  = Int

-- | ASCII art height in characters
type Height = Int

-- | The URL or filepath of the image to convert
type Url    = String


-- | Pymble can be started as a telnet server or output
-- the generated ASCII art directly to terminal.
--
data StartupMode
  = TelnetServer Port
  | DirectConvert (Maybe Width) (Maybe Height) (Maybe ColorScheme) Url
  deriving (Show, Eq)


-- | Parses the pymble startup mode.
--
startupMode :: Parser StartupMode
startupMode = directConvert <|> telnetServer


-- | The parser for 'TelnetServer' startup mode.
--
telnetServer :: Parser StartupMode
telnetServer = TelnetServer <$> option auto
    (  long    "port"
    <> short   'p'
    <> help    "Telnet server port"
    <> value   23
    <> metavar "INT"
    <> showDefault )


-- | The parser for 'DirectConvert' startup mode.
--
directConvert :: Parser StartupMode
directConvert = DirectConvert
 <$> (optional $ option auto $
           long    "width"
        <> short   'w'
        <> help    "ASCII art width"
        <> metavar "INT" )

 <*> (optional $ option auto $
           long    "height"
        <> short   'h'
        <> help    "ASCII art height"
        <> metavar "INT" )

 <*> (optional $ option auto $
           long    "color"
        <> short   'c'
        <> help    "ASCII art color palette"
        <> metavar "16|256|gs|tc" )

 <*> (strArgument $ 
           metavar "URL" 
        <> help    "URL or filepath of the image to convert" )