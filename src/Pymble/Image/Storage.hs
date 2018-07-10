{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    , defLoadHandlers
    -- * General facade
    , load
    -- * Concrete loaders
    , download
    , read
    ) where

import Codec.Picture (readImage, decodeImage, DynamicImage)
import Control.Exception (throwIO)
import Control.Monad.Catch (catch, Exception(..), Handler(..), SomeException(..))
import Data.Typeable (cast)
import Network.HTTP.Conduit (responseStatus, HttpExceptionContent(..))
import Network.HTTP.Simple (httpBS, parseRequest, getResponseBody, getResponseStatus, HttpException(..))
import Network.HTTP.Types.Status (Status(..))
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
  ConnectionFailureException { _cfeUrl :: String }
  deriving (Show)

instance Exception ConnectionFailureException where
  toException   = imageRetrievalExceptionToException
  fromException = imageRetrievalExceptionFromException

-- | General failure.
--
data RetrievalFailureException =
  RetrievalFailureException { _rfeRawException :: String }
  deriving (Show)

instance Exception RetrievalFailureException where
  toException   = imageRetrievalExceptionToException
  fromException = imageRetrievalExceptionFromException


-- | Default set of exception handlers for 'load' function.
-- The exception handlers convert exception to the default string representation.
--
defLoadHandlers :: [Handler IO (Either String a)]
defLoadHandlers =
    [ Handler $ \(ex :: MalformedUriException) ->
        msg $ "Malformed URL: \"" ++ _mueUri ex ++ "\""

    , Handler $ \(ex :: InvalidUriException) ->
        msg $ "Invalid URL: \"" ++ _iueUri ex ++ "\""

    , Handler $ \(ex :: ImageDecodeException) ->
        msg $ "Unable to decode the image: unknown format"

    , Handler $ \(ex :: DownloadFailureException) ->
        msg $ concat [ "Failed to download the image ("
                      , show $ _dfeCode ex
                      , " "
                      , _dfeMsg ex
                      , ")"
                      ]

    , Handler $ \(ex :: ConnectionFailureException) ->
        msg $ "Unable to connect to the remote server: \""
                ++ _cfeUrl ex ++ "\""

    , Handler $ \(ex :: RetrievalFailureException) ->
        msg $ "Unable to load the image: " ++ _rfeRawException ex
    ]
  where
    msg = return . Left


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
read path = do
  maybeImage <- readImage path
  case maybeImage of
    Right i -> return i
    Left  m -> throwIO $ ImageDecodeException m


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

    _ <- case getResponseStatus response of
          (Status 200  _  ) -> return ()
          (Status code msg) ->
            throwIO $ DownloadFailureException code (show msg)

    case decodeImage (getResponseBody response) of
      Right i -> return i
      Left  m -> throwIO $ ImageDecodeException m
  where 
    -- General hi-level handler
    handler = \case
      HttpExceptionRequest _ status ->
        handleHttpException status
      
      InvalidUrlException _ _ ->
        throwIO $ InvalidUriException url

      ex -> throwIO $ RetrievalFailureException $ show ex

    -- Http specific handler
    handleHttpException = \case
        StatusCodeException res _ ->
          let (Status code msg) = responseStatus res 
          in throwIO $ DownloadFailureException code (show msg)

        ConnectionFailure _ ->
          throwIO $ ConnectionFailureException url

        ex -> throwIO $ RetrievalFailureException $ show ex
        