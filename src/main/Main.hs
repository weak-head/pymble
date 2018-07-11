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
      <> header "pymble - image to ASCII art converter" )
   
    -- app version
    version = infoOption "1.0.0" (long "version" <> help "Show pymble version")

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
    hint "Loading image..."
    maybeImage <- (normalize <$> load url)
                    `catches` defLoadHandlers

    case maybeImage of
      Right image -> do
        let isize@(iw, ih)  = imageSize image
            (width, height) = adviceSize isize mWidth mHeight
            color           = maybe Color16 id mColor
            delayedArt      = toDelayedAsciiArt width height courierFull image

        hint $ concat [ "Converting "
                      , show iw ++ "x" ++ show ih ++ " image to "
                      , show width ++ "x" ++ show height ++ " ASCII art..."
                      ]
        coloredArt <- evalAsTerminalColor color delayedArt

        nl 
        putStr $ prettyPrint coloredArt ""
        nl 

      Left err -> do
        termMsgIO Error err
  where 
    hint = termMsgIO Hint
    nl   = putStrLn "\n"


-- | Bootstrap telnet server.
--
runTelnetServer :: Int -> IO ()
runTelnetServer port =
  startServer $ def { _appServerPort = port }