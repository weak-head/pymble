{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- This is the main module for image processing and conversion.
--
-- The module exposes normalization of dynamic images into classical 8-bit RGBA images,
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
    , Brightness
    , BrightnessMap
    , toDelayedAsciiArt
    , toUnboxedAsciiArt
    , bestFit
    -- * Repa conversion
    , toArray
    , fromArray
    -- * Image normalization and unification
    , normalize
    , unifyColor
    , averageBrightness
    ) where

import           Codec.Picture       as P
import           Codec.Picture.Types as PT
import           Data.Array.Repa     as R   hiding (map)
import qualified Data.Map.Lazy       as ML
import           Data.Word
import           Data.List (foldl')
import           Data.Foldable (minimumBy)
import           Data.Function (on)
import           Data.Tuple (swap)
----------------------------------------------------------------------


-- | Classical 8-bit RGBA image.
--
-- Each pixel has four 8-bit channels: red, green, blue and alpha.
--
type RGBAImage = P.Image P.PixelRGBA8


-- | The delayed two-dimensional array of arbitrary value.
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
                  -> BrightnessMap            -- ^ character brightness map
                  -> RGBAImage                -- ^ the image to convert to ASCII art
                  -> DelayedArray ColoredChar -- ^ ASCII art represented as 'DelayedArray'
toDelayedAsciiArt width height bmap img =
  let arr               = toArray img
      Z :. arrW :. arrH = R.extent arr
      -- it pretty common to have width to heigh ratio
      -- around 0.4 ~ 0.55, but this is very font
      -- specific, so it's up to the caller to figure that out
      -- and provide the width and the height of
      -- the resulting ASCII art that make sense.
      -- We are just following the spec here.
      charAreaWidth  = arrW `div` width
      charAreaHeight = arrH `div` height
  in R.traverse arr
      -- the shape of the ASCII art
      (\_ -> Z :. width :. height)

      -- Finds the color and the appropriate char to be used
      -- to represent the dedicated sub-area of the image
      (\lookup (Z :. w :. h) ->
        let coords = coordinates charAreaWidth charAreaHeight w h
            rgbas  = map (\(x, y) -> lookup (Z :. x :. y)) coords
        in (bestFit bmap $ averageBrightness rgbas, unifyColor rgbas))
  where
    -- the complete set of coordinates of the image area
    -- that is being covered by the character with position (chX, chY)
    coordinates areaW areaH chX chY =
      [(x, y) | x <- [(chX * areaW) .. (chX * areaW + areaW - 1)]
              , y <- [(chY * areaH) .. (chY * areaH + areaH - 1)]]


-- | Given a list of 'RGBA8' colors unifies them
-- into single average 'RGBA8' color. Alpha is not
-- affected by the unification and always remains full opaque.
--
unifyColor :: [RGBA8] -> RGBA8
unifyColor = 
    unwrapColor . foldl' accumulateColor ((0, 0, 0, 0), 0)
  where
    -- accumulates colors, preserving the number of colors accumulated
    accumulateColor ((nr, ng, nb, na), n) (r, g, b, _) =
      let r' = nr `seq` (nr + fromIntegral r)
          g' = ng `seq` (ng + fromIntegral g)
          b' = nb `seq` (nb + fromIntegral b)
          n' = n  `seq` (n + 1)
      in ((r', g', b', 255), n')

    -- unwraps the accumulated colors into single average RGBA8 color
    unwrapColor ((r, g, b, a), n) | n == 0 = (0, 0, 0, 0)
    unwrapColor ((r, g, b, a), n) =
      let r' = fromIntegral $ r `div` n
          g' = fromIntegral $ g `div` n
          b' = fromIntegral $ b `div` n
          a' = fromIntegral $ a
      in (r', g', b', a')


-- | Given a list of 'RGBA8' colors evaluates the
-- average brightness of the area formed by the pixels.
-- Alpha is not included into the computation and doesn't
-- affect the resulting brightness.
--
averageBrightness :: [RGBA8] -> Brightness
averageBrightness =
    unwrapBrightness . foldl' accumulateBrightness (0, 0)
  where
    -- accumulates average brightness, preserving the number of
    -- the accumulated items
    accumulateBrightness (nu, n) (r, g, b, a) =
      let rgb = (fromIntegral r + fromIntegral g + fromIntegral b) :: Integer
          nu' = nu `seq` (nu + (rgb `div` 3))
          n'  = n  `seq` (n + 1)
      in (nu', n')

    -- unwraps the accumulated total brightness and computes the total average
    unwrapBrightness (nu, n) | n == 0 = 0
    unwrapBrightness (nu, n) = fromIntegral $ nu `div` n


-- | Having the 'BrightnessMap', finds the
-- character that is the best fit for the given 'Brightness'.
bestFit :: BrightnessMap -> Brightness -> Char
bestFit bmap brightness | null bmap = error "empty brightness map"
bestFit bmap brightness =
    snd $ ML.foldrWithKey pickBest (head $ ML.assocs bmap) bmap
  where
    -- compare the current best fit with the
    -- new candidate and pick the best of them
    pickBest b c best = minimumBy (compare `on` (score . fst)) [(b, c), best]

    -- scores a bestfit brightness candidate
    score candidate = abs $ (fromIntegral brightness) - (fromIntegral candidate)


-- | From ASCII art point of view this is basically the same as 'toDelayedAsciiArt',
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
  R.fromFunction (Z :. imageWidth :. imageHeight)
    (\(Z :. x :. y) ->
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
    Z :. width :. height = R.extent array
    -- given the array coordinates creates corresponding image pixel
    mkPixel x y =
      let (r, g, b, a) = array ! (Z :. x :. y)
      in P.PixelRGBA8 r g b a
      