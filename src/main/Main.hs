{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- |
--
module Main (main) where

import Data.Default
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (maybe)

import Pymble.AppConfig
import Pymble.Telnet.Server (startServer)

import Data.Maybe (fromJust)
import Pymble.Image.HTTP (download)
import Pymble.Image.Convert (normalize, toDelayedAsciiArt)
import Pymble.Image.Fontspec (courierFull)
import Pymble.Image.Helpers (adviceSize)
import Pymble.PrettyPrint.Telnet (evalAsTerminalColor, encodeColoredString, prettyPrint, termClear, ColorScheme(..))
import qualified Pymble.PrettyPrint.Telnet.Color as TC

import Pymble.Args (startupMode, StartupMode(..))
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


-- | Dirty direct convert for testing purposes.
--
runDirectConvert :: StartupMode -> IO ()
runDirectConvert (DirectConvert mWidth mHeight mColor url) = do

  -- todo: replace with utility calls
  putStrLn $ encodeColoredString (TC.Color16 3) "Reading image..." ""
  maybeImage <- normalize <$> download url

  case maybeImage of

    Just image -> do
      let (width, height) = adviceSize mWidth mHeight
          color           = maybe Color16 id mColor
          delayedArt      = toDelayedAsciiArt width height courierFull image

      putStrLn $ encodeColoredString (TC.Color16 3) "Converting image..." ""
      coloredArt <- evalAsTerminalColor color delayedArt

      let printedArt = prettyPrint coloredArt
          cleanPrint = termClear . printedArt

      putStrLn $ cleanPrint ""

    Nothing -> do
      putStrLn $ encodeColoredString (TC.Color16 1) "failed to retrieve image" ""


-- | Dirty bootstrap of the telnet server.
--
runTelnetServer :: Int -> IO ()
runTelnetServer port =
  startServer $ def { _appServerPort = port }