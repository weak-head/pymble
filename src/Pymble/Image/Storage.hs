{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Control.Exception as E
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.URI (isURI)
import System.Directory (doesFileExist)
----------------------------------------------------------------------

-- | A facade for loading and decoding images
-- from both local and remote sources.
--
load :: String -> IO DynamicImage
load url = do
  isFile <- doesFileExist url
  case isFile of
    True  -> read url
    False -> download url


-- | Read the image from the file system
-- and decode it to 'P.DynamicImage'.
--
read :: String -> IO DynamicImage
read path =
  join $ either error return <$> readImage path


-- | Download the image over HTTP
-- and decode it to 'P.DynamicImage'.
--
download :: String -> IO DynamicImage
download url = do

    _ <- if isURI url
          then return ()
          else error "Failed to parse the URI"

    response <- (parseRequest url >>= httpBS) 
                  `E.catch` handler

    case decodeImage (getResponseBody response) of
      Right i -> return i
      Left  _ -> error "Failed to decode the image"
  where 
    handler = \case
      HttpExceptionRequest _ (StatusCodeException res _) ->
        let (Status code msg) = responseStatus res 
        in error $ concat [ "Failed to download the image ("
                          , show code
                          , " "
                          , show msg
                          , ")"]

      HttpExceptionRequest _ (ConnectionFailure _) ->
        error "Failed to download the image: Connection failure"
      
      InvalidUrlException _ _ ->
        error "Failed to download the image: Invalid URL"

      _ -> error "Failed to download the image"
