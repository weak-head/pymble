{-# LANGUAGE OverloadedStrings #-}
-- |
--
module Pymble.Args
    ( 
    -- *
      StartupMode(..)
    , Width
    , Height
    , Url
    , startupMode
    -- *
    , telnetServer
    , directConvert
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

import Pymble.AppConfig (Port)
import Pymble.PrettyPrint.Telnet (ColorScheme(..))
----------------------------------------------------------------------

-- | ASCII art width in characters
type Width  = Int

-- | ASCII art height in characters
type Height = Int

-- | The URL or filepath of the image to convert
type Url    = String


-- |
--
data StartupMode
  = TelnetServer Port
  | DirectConvert (Maybe Width) (Maybe Height) (Maybe ColorScheme) Url
  deriving (Show)


-- |
--
startupMode :: Parser StartupMode
startupMode = directConvert <|> telnetServer


-- |
--
telnetServer :: Parser StartupMode
telnetServer = TelnetServer <$> option auto
    (  long    "port"
    <> short   'p'
    <> help    "Telnet server port"
    <> value   23
    <> metavar "INT"
    <> showDefault )


-- |
--
directConvert :: Parser StartupMode
directConvert = DirectConvert
 <$> (optional $ option auto $
           long    "width"
        <> short   'w'
        <> help    "Width of the generated ASCII art (char)"
        <> metavar "INT" )

 <*> (optional $ option auto $
           long    "height"
        <> short   'h'
        <> help    "Height of the generated ASCII art (char)"
        <> metavar "INT" )

 <*> (optional $ option auto $
           long    "color"
        <> short   'c'
        <> help    "Color scheme of the generated ASCII art"
        <> metavar "(16|256|grayscale|truecolor)" )

 <*> (strArgument $ 
           metavar "URL" 
        <> help    "URL or filepath of the image to convert" )