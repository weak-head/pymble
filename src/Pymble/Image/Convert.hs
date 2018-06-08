{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Pymble.Image.Convert
    (
    -- * Common types
      RGBAImage
    , DelayedArray
    , UnboxedArray
    -- * ASCII conversion
    , ColoredChar
    , RGBA8
    , toDelayedAsciiArray
    , toUnboxedAsciiArray
    -- * Repa conversion
    , toArray
    , fromArray
    -- * Format normalization 
    , normalize
    
    ) where

import Codec.Picture       as P
import Codec.Picture.Types as PT
import Data.Array.Repa     as R
import Data.Array.Repa ((:.), (!))
----------------------------------------------------------------------


-- |
--
type RGBAImage = P.Image P.PixelRGBA8


-- |
--
type DelayedArray a = R.Array R.D R.DIM2 a


-- |
--
type UnboxedArray a = R.Array R.U R.DIM2 a


-- | Classical pixel reprsentation (8bit red, green, blue and alpha)
-- in form of tuple.
--
-- This pixel representation is used when an 'P.Image' is converted
-- into 'R.Array' for further parallel processing.
--
type RGBA8 = (P.Pixel8, P.Pixel8, P.Pixel8, P.Pixel8)


-- |
--
type ColoredChar = (Char, RGBA8) 


-- |
--
toDelayedAsciiArray :: Int -> Int -> RGBAImage -> DelayedArray ColoredChar
toDelayedAsciiArray width height img =
  R.traverse (toArray img)
    -- the shape of the resulting array
    (\_ -> R.Z :. width :. height)

    -- Finds the color and the appropriate char to be used
    -- to represend the dedicated sub-area of the image
    (\lookup (R.Z :. w :. h) ->
      let
          (r, g, b, a) = lookup (Z :. w :. h)
      in
          (undefined, undefined)
      )


-- |
--
toUnboxedAsciiArray :: Int -> Int -> RGBAImage -> IO (UnboxedArray ColoredChar)
toUnboxedAsciiArray width height img = R.computeP $ toDelayedAsciiArray width height img


-- | Converts a generic image of a loosely defined format
-- to the 'P.PixelRGBA8' image with the classical
-- pixels (8bit red, green, blue and alpha).
--
normalize :: P.DynamicImage -> Maybe RGBAImage
normalize dynamicImage =
  case dynamicImage of
    P.ImageY8     i -> Just $ PT.promoteImage i
    P.ImageYA8    i -> Just $ PT.promoteImage i
    P.ImageRGB8   i -> Just $ PT.promoteImage i
    P.ImageRGBA8  i -> Just i
    P.ImageYCbCr8 i -> Just $ PT.promoteImage (PT.convertImage i :: P.Image P.PixelRGB8)
    P.ImageCMYK8  i -> Just $ PT.promoteImage (PT.convertImage i :: P.Image P.PixelRGB8)
    _               -> Nothing


-- | Converts the 'P.PixelRBGA8' 'P.Image' to the
-- repa 'R.Array' of 'RGBA8' pixels
-- that could be processed in parallel.
--
-- This operation is opposite and symmetrical to 'fromArray'.
--
toArray :: RGBAImage -> DelayedArray RGBA8
toArray img@Image {..} =
  R.fromFunction (R.Z :. imageWidth :. imageHeight)
    (\(R.Z :. x :. y) ->
      let (P.PixelRGBA8 r g b a) = P.pixelAt img x y
      in (r, g, b, a))


-- | Converts an 'R.Array' of 'RGBA8' pixels to the 
-- 'P.Image' with 'P.PixelRGBA8' pixels.
--
-- This operation is opposite and symmetrical to 'toArray'.
--
fromArray :: UnboxedArray RGBA8 -> RGBAImage
fromArray array =
    P.generateImage mkPixel width height
  where
    -- image size
    R.Z :. width :. height = R.extent array
    -- given the array coordinates creates corresponding image pixel
    mkPixel x y =
      let (r, g, b, a) = array ! (Z :. x :. y)
      in P.PixelRGBA8 r g b a
      