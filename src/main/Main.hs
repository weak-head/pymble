{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- |
--
module Main (main) where

import Data.Default
import Data.Maybe (fromJust)
import Data.Maybe (maybe)
import Data.Semigroup ((<>))
import Control.Exception (catch) 
import Options.Applicative

import           Pymble.AppConfig
import           Pymble.Args
import           Pymble.Image.Convert (normalize, toDelayedAsciiArt)
import           Pymble.Image.Fontspec
import           Pymble.Image.Helpers
import           Pymble.Image.Storage (load)
import           Pymble.PrettyPrint.Telnet
import qualified Pymble.PrettyPrint.Telnet.Color as TC
import           Pymble.Telnet.Server (startServer)

----------------------------------------------------------------------


-- | Application Main
--
main :: IO ()
main = runApp =<< execParser optsParser
  where
    -- configure arg parser
    optsParser = info (helper' <*> version <*> startupMode)
      (  fullDesc
      <> progDesc "image to ASCII art converter (tbd)"
      <> header "pymble - telnet server that converts images to ASCII art (tbd)" )
   
    -- app version
    version = infoOption "0.9" (long "version" <> help "Show version")

    -- the default helper binds '-h' to show help info,
    -- but we want to use '-h' exclusively for height
    helper' = abortOption ShowHelpText (long "help" <> help "Show this help text")


-- |
--
runApp :: StartupMode -> IO ()
runApp = \case
  ts@(TelnetServer port)     -> runTelnetServer port 
  dc@(DirectConvert _ _ _ _) -> runDirectConvert dc


-- | Convert image to ASCII art and outputs the art to terminal.
--
runDirectConvert :: StartupMode -> IO ()
runDirectConvert (DirectConvert mWidth mHeight mColor url) = do

  termMsgIO Info "Loading image..."
  image <- (fromJust . normalize <$> load url)

  let (width, height) = adviceSize (imageSize image) mWidth mHeight
      color           = maybe Color16 id mColor
      delayedArt      = toDelayedAsciiArt width height courierFull image

  termMsgIO Info "Converting image..."
  coloredArt <- evalAsTerminalColor color delayedArt

  putStrLn "\n"
  putStrLn $ prettyPrint coloredArt ""


-- | Dirty bootstrap of the telnet server.
--
runTelnetServer :: Int -> IO ()
runTelnetServer port =
  startServer $ def { _appServerPort = port }