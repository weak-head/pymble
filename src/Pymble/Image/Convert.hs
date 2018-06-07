{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Pymble.Image.Convert
    (
    -- * Repa conversion
      toArray
    , fromArray
    -- * Format normalization 
    , normalize
    
    ) where

import Codec.Picture       as P
import Codec.Picture.Types as PT
import Data.Array.Repa     as R
import Data.Array.Repa ((:.), (!))

----------------------------------------------------------------------

-- | Classical pixel reprsentation (8bit red, green, blue and alpha)
-- in form of tuple.
--
-- This pixel representation is used when an 'P.Image' is converted
-- into 'R.Array' for further parallel processing.
--
type RGBA8 = (P.Pixel8, P.Pixel8, P.Pixel8, P.Pixel8)


-- | Converts generic image of undefined format
-- to the RGBA8 image with the classical
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


-- | Converts RBGA8 'P.Image' to the 'R.Array' of 'RGBA8' pixels
-- that could be processed in parallel using repa.
-- This operation is opposite and symmetrical to 'fromArray'.
--
toArray :: P.Image P.PixelRGBA8 -> R.Array R.D R.DIM2 RGBA8
toArray img@Image {..} =
  R.fromFunction (R.Z :. imageWidth :. imageHeight)
    (\(R.Z :. x :. y) ->
      let (P.PixelRGBA8 r g b a) = P.pixelAt img x y
      in (r, g, b, a))


-- | Converts an 'R.Array' of 'RGBA8' pixels to 
-- RGBA8 'P.Image' with classical pixels.
-- This operation is opposite and symmetrical to 'toArray'.
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
      