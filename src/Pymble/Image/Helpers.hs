{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pymble.Image.Helpers
  (
  -- * Advice ASCII art width and height
    ArtWidth
  , ArtHeight
  , ImageWidth
  , ImageHeight
  , FontAspectRatio
  , adviceSize
  , adviceSizeWithRatio
  -- * Image helpers
  , imageSize
  ) where

import qualified Codec.Picture as P
----------------------------------------------------------------------

-- | ASCII art width (in characters).
type ArtWidth  = Int

-- | ASCII art height (in characters).
type ArtHeight = Int

-- | Image width
type ImageWidth = Int

-- | Image height
type ImageHeight = Int

-- | Font aspect ratio.
type FontAspectRatio = Double


-- | Optionally given the ASCII art width and height
-- advices the new width and heigh that will keep the
-- image proportions as much correct as possible.
--
-- The 'adviceSize' uses the default font
-- aspect ratio of 0.58 to advice the ASCII art
-- width and height.
--
adviceSize :: (ImageWidth, ImageHeight)
           -> Maybe ArtWidth
           -> Maybe ArtHeight
           -> (ArtWidth, ArtHeight)
adviceSize = adviceSizeWithRatio 0.58


-- | Optionally given the ASCII art width and height
-- advices the new width and height based on the
-- custom aspect ratio.
--
-- Common font aspect ratios:
--     Arial              0.52
--     Avant Garde        0.45
--     Bookman            0.40
--     Calibri            0.47
--     Century Schoolbook 0.48
--     Cochin             0.41
--     Comic Sans         0.53
--     Courier            0.43
--     Courier New        0.42
--     Garamond           0.38
--     Georgia            0.48
--     Helvetica          0.52
--     Palatino           0.42
--     Tahoma             0.55
--     Times New Roman    0.45
--     Trebuchet          0.52
--     Verdana            0.58
--
adviceSizeWithRatio :: FontAspectRatio
                    -> (ImageWidth, ImageHeight)
                    -> Maybe ArtWidth
                    -> Maybe ArtHeight
                    -> (ArtWidth, ArtHeight)
adviceSizeWithRatio ratio _ _ _
  | ratio <= 0 = error "ratio should be greater than zero"

adviceSizeWithRatio _ (iw, ih) _ _
  | iw <= 0 = error "image width should be greater than zero"
  | ih <= 0 = error "image height should be greater than zero"

adviceSizeWithRatio ratio isize Nothing  Nothing =
  adviceSizeWithRatio ratio isize (Just 80) Nothing 

adviceSizeWithRatio ratio (iw, ih) (Just w) Nothing =
  let iwhRatio = (fromIntegral iw) / (fromIntegral ih)
      aspectH  = (fromIntegral w) * ratio
      realH    = aspectH / iwhRatio
  in (w, round realH)

adviceSizeWithRatio ratio (iw, ih) Nothing  (Just h) =
  let iwhRatio = (fromIntegral iw) / (fromIntegral ih)
      aspectW  = (fromIntegral h) / ratio
      realW    = aspectW * iwhRatio
  in (round realW, h)

adviceSizeWithRatio ratio (iw, ih) (Just w) (Just h) =
  (w, h)


-- | Get the width and the height of the image.
--
imageSize :: P.Image a -> (ImageWidth, ImageHeight)
imageSize P.Image {..} = (imageWidth, imageHeight)