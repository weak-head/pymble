{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module Pymble.Image.Storage
    (
    -- * Exceptions
      ImageRetrievalException(..)
    , MalformedUriException(..)
    , InvalidUriException(..)
    , ImageDecodeException(..)
    , DownloadFailureException(..)
    , ConnectionFailureException(..)
    , RetrievalFailureException(..)
    -- * General facade
    , load
    -- * Concrete loaders
    , download
    , read
    ) where

import Codec.Picture
import Control.Exception
import Control.Monad (join)
import Data.Typeable
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.URI (isURI)
import Prelude hiding (read)
import System.Directory (doesFileExist)

import Pymble.Exception
----------------------------------------------------------------------

-- | Base class for all image retrieval related exceptions.
--
data ImageRetrievalException = forall e . Exception e => ImageRetrievalException e

instance Show ImageRetrievalException where
  show (ImageRetrievalException e) = show e

instance Exception ImageRetrievalException where
  toException   = pymbleExceptionToException
  fromException = pymbleExceptionFromException

imageRetrievalExceptionToException :: Exception e => e -> SomeException
imageRetrievalExceptionToException = toException . ImageRetrievalException

imageRetrievalExceptionFromException :: Exception e => SomeException -> Maybe e
imageRetrievalExceptionFromException x = do
  ImageRetrievalException e <- fromException x
  cast e


-- | URI is malformed.
--
data MalformedUriException =
  MalformedUriException { _mueUri :: String }
  deriving (Show)

instance Exception MalformedUriException where
  toException   = imageRetrievalExceptionToException
  fromException = imageRetrievalExceptionFromException

-- | URI is invalid.
--
data InvalidUriException =
  InvalidUriException { _iueUri :: String }
  deriving (Show)

instance Exception InvalidUriException where
  toException   = imageRetrievalExceptionToException
  fromException = imageRetrievalExceptionFromException

-- | Unable to decode the image.
--
data ImageDecodeException =
  ImageDecodeException { _ideMessage :: String }
  deriving (Show)

instance Exception ImageDecodeException where
  toException   = imageRetrievalExceptionToException
  fromException = imageRetrievalExceptionFromException

-- | Failed to download the image.
--
data DownloadFailureException =
  DownloadFailureException { _dfeCode :: Int, _dfeMsg :: String }
  deriving (Show)

instance Exception DownloadFailureException where
  toException   = imageRetrievalExceptionToException
  fromException = imageRetrievalExceptionFromException

-- | Failed to connect to the server.
--
data ConnectionFailureException =
  ConnectionFailureException
  deriving (Show)

instance Exception ConnectionFailureException where
  toException   = imageRetrievalExceptionToException
  fromException = imageRetrievalExceptionFromException

-- | General failure.
--
data RetrievalFailureException =
  RetrievalFailureException
  deriving (Show)

instance Exception RetrievalFailureException where
  toException   = imageRetrievalExceptionToException
  fromException = imageRetrievalExceptionFromException

----------------------------------------


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
          else throwIO $ MalformedUriException url

    response <- (parseRequest url >>= httpBS) 
                  `catch` handler

    case decodeImage (getResponseBody response) of
      Right i -> return i
      Left  m -> throwIO $ ImageDecodeException m
  where 
    handler = \case
      HttpExceptionRequest _ (StatusCodeException res _) ->
        let (Status code msg) = responseStatus res 
        in throwIO $ DownloadFailureException code (show msg)

      HttpExceptionRequest _ (ConnectionFailure _) ->
        throwIO ConnectionFailureException
      
      InvalidUrlException _ _ ->
        throwIO $ InvalidUriException url

      _ -> throwIO RetrievalFailureException