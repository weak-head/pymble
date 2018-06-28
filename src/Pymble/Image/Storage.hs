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
import Codec.Picture       as P
import Network.HTTP.Simple as NW
import Network.URI (isURI)
import System.Directory (doesFileExist)
----------------------------------------------------------------------

-- | A facade for loading images from both 
-- local and remote sources.
--
load :: String -> IO P.DynamicImage
load url = do
  isFile <- doesFileExist url
  case isFile of
    True  -> read url
    False -> if isURI url
               then download url
               else error "invalid url or path"


-- | Read and load an image file, converting the image
-- to 'P.DynamicImage'.
--
read :: String -> IO P.DynamicImage
read path =
  join $ either error return <$> P.readImage path


-- | Downloads an image with the specified URI
-- and converts it to 'P.DynamicImage'.
--
download :: String -> IO P.DynamicImage
download uri = do
  request <- NW.parseRequest uri
  response <- NW.httpBS request 
  
  let statusCode  = NW.getResponseStatusCode response
      contentType = NW.getResponseHeader "Content-Type" response
      body        = NW.getResponseBody response
  
  either error return $ decodeImage body