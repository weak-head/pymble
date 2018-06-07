{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pymble.Image (
    download
  , normalize

) where

import Codec.Picture       as P
import Codec.Picture.Types as PT
import Data.Array.Repa     as R
import Data.Array.Repa ((:.), (!))
import Network.HTTP.Simple as NW

----------------------------------------------------------------------

type RGBA8 = (P.Pixel8, P.Pixel8, P.Pixel8, P.Pixel8)


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


-- | Converts 'P.Image' to repa 'R.Array' for further processing.
--
toArray :: P.Image P.PixelRGBA8 -> R.Array R.D R.DIM2 RGBA8
toArray img@Image {..} =
  R.fromFunction (R.Z :. imageWidth :. imageHeight)
    (\(R.Z :. x :. y) ->
      let (P.PixelRGBA8 r g b a) = P.pixelAt img x y
      in (r, g, b, a))


-- | Converts repa 'R.Array' to resulting 'P.Image'.
--
fromArray :: R.Array R.U R.DIM2 RGBA8 -> P.Image P.PixelRGBA8
fromArray array =
    P.generateImage mkPixel width height
  where
    -- image size
    R.Z :. width :. height = R.extent array
    -- given the array coordinates creates corresponding image pixel
    mkPixel x y =
      let (r, g, b, a) = array ! (Z :. x :. y)
      in P.PixelRGBA8 r g b a
      