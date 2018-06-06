{-# LANGUAGE OverloadedStrings #-}

module Pymble.Image (
    download
  , normalize

) where

import Codec.Picture       as P
import Codec.Picture.Types as PT
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


-- | Converts generic image to image with the classical
-- pixels (8bit red, green, blue and alpha).
--
normalize :: P.DynamicImage -> Maybe (P.Image P.PixelRGBA8)
normalize dynamicImage =
  case dynamicImage of
    P.ImageY8     i -> Just $ PT.promoteImage i
    P.ImageYA8    i -> Just $ PT.promoteImage i
    P.ImageRGB8   i -> Just $ PT.promoteImage i
    P.ImageRGBA8  i -> Just i
    P.ImageYCbCr8 i -> Just $ PT.promoteImage (PT.convertImage i :: P.Image P.PixelRGB8)
    P.ImageCMYK8  i -> Just $ PT.promoteImage (PT.convertImage i :: P.Image P.PixelRGB8)
    _               -> Nothing