{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- |
--
module Main (main) where

import Control.Monad.Catch (catches)
import Data.Default (def)
import Data.Maybe (maybe)
import Data.Semigroup ((<>))
import Options.Applicative

import Pymble.AppConfig (AppConfig(..))
import Pymble.Args (startupMode, StartupMode(..))
import Pymble.Image.Convert (normalize, toDelayedAsciiArt)
import Pymble.Image.Fontspec (courierFull)
import Pymble.Image.Helpers (adviceSize, imageSize)
import Pymble.Image.Storage (load, defLoadHandlers)
import Pymble.PrettyPrint.Terminal
import Pymble.Telnet.Server (startServer)

----------------------------------------------------------------------


-- | Application entry point.
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
    version = infoOption "0.1.0-alpha" (long "version" <> help "Show version")

    -- the default helper binds '-h' to show help info,
    -- but we want to use '-h' exclusively for height
    helper' = abortOption ShowHelpText (long "help" <> help "Show this help text")


-- | Run the application based on the startup mode.
--
runApp :: StartupMode -> IO ()
runApp = \case
  ts@(TelnetServer port)     -> runTelnetServer port 
  dc@(DirectConvert _ _ _ _) -> runDirectConvert dc


-- | Convert the image to ASCII art and print the art directly to terminal.
--
runDirectConvert :: StartupMode -> IO ()
runDirectConvert (DirectConvert mWidth mHeight mColor url) =
  do
    termMsgIO Info "Loading image..."
    maybeImage <- (normalize <$> load url) `catches` defLoadHandlers

    case maybeImage of
      Right image -> do
        let (width, height) = adviceSize (imageSize image) mWidth mHeight
            color           = maybe Color16 id mColor
            delayedArt      = toDelayedAsciiArt width height courierFull image

        termMsgIO Info "Generating ASCII art..."
        coloredArt <- evalAsTerminalColor color delayedArt

        putStrLn "\n" >> (putStrLn $ prettyPrint coloredArt "") >> putStrLn "\n"

      Left err -> termMsgIO Error err


-- | Bootstrap telnet server.
--
runTelnetServer :: Int -> IO ()
runTelnetServer port =
  startServer $ def { _appServerPort = port }