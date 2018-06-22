module Main (main) where

import Data.Default

import Pymble.AppConfig
import Pymble.Telnet.Server (startServer)

import Data.Maybe (fromJust)
import Pymble.Image.HTTP (download)
import Pymble.Image.Convert (normalize, toDelayedAsciiArt)
import Pymble.Image.Fontspec (courierFull)
import Pymble.PrettyPrint.Telnet (evalAsTerminalColor, prettyPrint, termClear, ColorScheme(..))


-- | Application Main
--
main :: IO ()
main = do
  runTest  
  --startServer def


-- Just a dirty example to check the logic
runTest :: IO ()
runTest = do
  let url = "https://media.istockphoto.com/photos/red-apple-picture-id495878092?k=6&m=495878092&s=612x612&w=0&h=q9k5jN-1giBGZgTM6QhyKkPqtGf6vRpkgDzAwEz9DkY="
  image <- fromJust . normalize <$> download url

  let delayedColoredArt = toDelayedAsciiArt 80 40 courierFull image 
  coloredArt <- evalAsTerminalColor Color16 delayedColoredArt

  putStrLn $ termClear . (prettyPrint coloredArt) $ ""

