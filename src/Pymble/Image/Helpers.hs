{-# LANGUAGE OverloadedStrings #-}

module Pymble.Image.Helpers
  (
  -- * Advice ASCII art width and height
    Width
  , Height
  , AspectRatio
  , adviceSize
  , adviceSizeWithRatio
  ) where

----------------------------------------------------------------------

-- | ASCII art width (in characters).
type Width  = Int

-- | ASCII art height (in characters).
type Height = Int

-- | Font aspect ratio.
type AspectRatio = Double


-- | Optionally given the ASCII art width and height
-- advices the new width and heigh that will keep the
-- image proportions as much correct as possible.
--
-- The 'adviceSize' uses the default font
-- aspect ratio of 0.58 to advice the ASCII art
-- width and height.
--
adviceSize :: Maybe Width
           -> Maybe Height
           -> (Width, Height)
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
adviceSizeWithRatio :: AspectRatio
                    -> Maybe Width
                    -> Maybe Height
                    -> (Width, Height)
adviceSizeWithRatio ratio _         _       | ratio <= 0 = error "ratio should be greater than zero"
adviceSizeWithRatio ratio Nothing  Nothing  = adviceSizeWithRatio ratio (Just 80) Nothing 
adviceSizeWithRatio ratio (Just w) Nothing  = (w, round $ (fromIntegral w) * ratio)
adviceSizeWithRatio ratio Nothing  (Just h) = (round $ (fromIntegral h) / ratio, h)
adviceSizeWithRatio ratio (Just w) (Just h) = (w, h)