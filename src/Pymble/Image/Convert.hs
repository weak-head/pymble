{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- This is the main module for image processing and conversion.
--
-- The module exposes normalization of dymamic images into classical 8-bit RGBA images,
-- provides access to conversions between an image and repa array and the most
-- important it allows converting images to colored ASCII art.
-- 
module Pymble.Image.Convert
    (
    -- * Common types
      RGBAImage
    , DelayedArray
    , UnboxedArray
    -- * ASCII conversion
    , ColoredChar
    , RGBA8
    , Brightness,
    , BrightnessMap
    , toDelayedAsciiArt
    , toUnboxedAsciiArt
    -- * Repa conversion
    , toArray
    , fromArray
    -- * Image normalization 
    , normalize
    
    ) where

import           Codec.Picture       as P
import           Codec.Picture.Types as PT
import           Data.Array.Repa     as R
import           Data.Array.Repa ((:.), (!))
import qualified Data.Map.Lazy       as ML
import           Data.Word
----------------------------------------------------------------------


-- | Classical 8-bit RGBA image.
--
-- Each pixel has four 8-bit channels: red, green, blue and alpha.
--
type RGBAImage = P.Image P.PixelRGBA8


-- | The delayed two-dimentional array of arbitrary value.
-- In the delayed array values are represented as
-- functions to element values.
--
-- Delayed arrays should be explicitly evaluated
-- in order to produce the actual value.
--
type DelayedArray a = R.Array R.D R.DIM2 a


-- | Fully evaluated and unboxed array.
--
type UnboxedArray a = R.Array R.U R.DIM2 a


-- | Classical 8-bit RGBA pixel in form of tuple.
--
-- This pixel representation is used when an 'RGBAImage' is converted
-- into 'DelayedArray' for further parallel processing. Another use-case
-- is representation of the character color.
--
type RGBA8 = (P.Pixel8, P.Pixel8, P.Pixel8, P.Pixel8)


-- | This tuple basically represents a char
-- that should be rendered with the specified
-- color.
--
type ColoredChar = (Char, RGBA8) 


-- | The average brightness of a dedicated area of an image or
-- a total average brightness of a specific character.
--
type Brightness = Word8


-- | The brightness map is used during the image
-- conversion to ASCII art. Every character has a relative
-- brightness that could be used to substitute a dedicated
-- area of an image. The brightness map defines a dictionary
-- that is used during the substitution. Different dictionaries
-- produce different image conversion result.
--
type BrightnessMap = ML.Map Brightness Char 


-- | Given the image and the specification,
-- converts the image to a colored ASCII art.
-- The ASCII art is represented as 'DelayedArray' of 'ColoredChar'
-- and, as a result, is not fully evaluated.
--
toDelayedAsciiArt :: Int                      -- ^ ASCII art width (in characters)
                  -> Int                      -- ^ ASCII art height (in characters)
                  -> BrightnessMap            -- ^ character brighness map
                  -> RGBAImage                -- ^ the image to convert to ASCII art
                  -> DelayedArray ColoredChar -- ^ ASCII art represented as 'DelayedArray'
toDelayedAsciiArt width height bmap img =
  R.traverse (toArray img)
    -- the shape of the resulting array
    (\_ -> R.Z :. width :. height)

    -- Finds the color and the appropriate char to be used
    -- to represend the dedicated sub-area of the image
    (\lookup (R.Z :. w :. h) ->
      let
          (r, g, b, a) = lookup (R.Z :. w :. h)
      in
          (undefined, undefined)
      )


-- | From ASCII art poing of view this is basically the same as 'toDelayedAsciiArt',
-- with the only difference is that the 'toUnboxedAsciiArt' produces a side effect of fully evaluating
-- and unboxing the 'DelayedArray' into 'UnboxedArray' using the parallel computation.
--
toUnboxedAsciiArt :: Int                            -- ^ ASCII art width (in characters)
                  -> Int                            -- ^ ASCII art height (in characters)
                  -> BrightnessMap                  -- ^ character brightness map
                  -> RGBAImage                      -- ^ the image to convert to ASCII art
                  -> IO (UnboxedArray ColoredChar)  -- ^ fully evaluated ASCII art
toUnboxedAsciiArt width height bmap img = R.computeP $ toDelayedAsciiArt width height bmap img


-- | Converts an image of a loosely defined format to the 'RGBAImage'
-- that has a classical 8-bit pixel representation and could be
-- further processed by other functions in this module.
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


-- | Converts the classical 8-bit 'RGBAImage' to the
-- 'DelayedArray' representation that is used to efficiently
-- process the image by taking advantage of the 
-- parallel computation and repa library.
--
-- This operation is opposite and symmetrical to 'fromArray'.
--
toArray :: RGBAImage -> DelayedArray RGBA8
toArray img@Image {..} =
  R.fromFunction (R.Z :. imageWidth :. imageHeight)
    (\(R.Z :. x :. y) ->
      let (P.PixelRGBA8 r g b a) = P.pixelAt img x y
      in (r, g, b, a))


-- | Backward conversion of the 'UnboxedArray' into classical
-- 8-bit 'RGBAImage'.
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
      let (r, g, b, a) = array ! (R.Z :. x :. y)
      in P.PixelRGBA8 r g b a
      