{-# LANGUAGE OverloadedStrings #-}

module Pymble.Image.HTTP
    (
      download
    ) where

import Codec.Picture       as P
import Network.HTTP.Simple as NW

----------------------------------------------------------------------

-- | Downloads the image with the specified URI
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