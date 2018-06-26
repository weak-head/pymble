{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- |
--
module Main (main) where

import Data.Default
import Options.Applicative
import Data.Semigroup((<>))

import Pymble.AppConfig
import Pymble.Telnet.Server (startServer)

import Data.Maybe (fromJust)
import Pymble.Image.HTTP (download)
import Pymble.Image.Convert (normalize, toDelayedAsciiArt)
import Pymble.Image.Fontspec (courierFull)
import Pymble.PrettyPrint.Telnet (evalAsTerminalColor, prettyPrint, termClear, ColorScheme(..))

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
  ts@(TelnetServer port)       -> print ts 
  dc@(DirectConvert w h c url) -> print dc 


-- Just a dirty example to check the logic
runTest :: IO ()
runTest = do
  let url = "https://media.istockphoto.com/photos/red-apple-picture-id495878092?k=6&m=495878092&s=612x612&w=0&h=q9k5jN-1giBGZgTM6QhyKkPqtGf6vRpkgDzAwEz9DkY="
  image <- fromJust . normalize <$> download url

  let delayedColoredArt = toDelayedAsciiArt 80 40 courierFull image 
  coloredArt <- evalAsTerminalColor Color16 delayedColoredArt

  putStrLn $ termClear . (prettyPrint coloredArt) $ ""


-- |
--
runTelnetServer :: Int -> IO ()
runTelnetServer port =
  startServer def