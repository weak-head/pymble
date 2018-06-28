{-# LANGUAGE OverloadedStrings #-}

module Pymble.Image.Storage
    (
    -- * General facade
      load
    -- * Concrete loaders
    , download
    , read
    ) where

import Prelude hiding (read)
import Control.Monad (join)
import Codec.Picture
import Network.HTTP.Simple
import System.Directory (doesFileExist)
----------------------------------------------------------------------

-- | A facade for loading images from both 
-- local and remote sources.
--
load :: String -> IO DynamicImage
load url = do
  isFile <- doesFileExist url
  case isFile of
    True  -> read url
    False -> download url


-- | Read and load an image file, converting the image
-- to 'P.DynamicImage'.
--
read :: String -> IO DynamicImage
read path =
  join $ either error return <$> readImage path


-- | Downloads an image with the specified URI
-- and converts it to 'P.DynamicImage'.
--
download :: String -> IO DynamicImage
download url = do
  response <- parseRequest url >>= httpBS

  case decodeImage (getResponseBody response) of
    Right i  -> return i
    Left err -> error err