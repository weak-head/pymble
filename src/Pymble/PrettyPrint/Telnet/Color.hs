{-# LANGUAGE OverloadedStrings #-}
-- |
--
module Pymble.PrettyPrint.Telnet.Color
    (
    -- * Common types
      TerminalColor(..)
    , Color
    -- * Color approximation
    , toStandard16Color
    , toX256Term
    , toGrayscale
    , toTrueColor
    -- * Utility functions
    , euclideanDistance
    ) where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Word (Word8)
----------------------------------------------------------------------

-- | Extended ANSI compatible terminal color.
--
--  <https://en.wikipedia.org/wiki/ANSI_escape_code#Colors ANSI colors>
--
data TerminalColor =
    Color16   !Word8                -- ^ Standard 16-color palette.
                                    --   Possible range: [0, 15]
  | X256term  !Word8                -- ^ Extended 256-color palette for x-term.
                                    --   Possible range: [0, 231]
  | Grayscale !Word8                -- ^ Grayscale color from the extended
                                    --   256-color palette for x-term.
                                    --   Possible range: [232, 255]
  | TrueColor !Word8 !Word8 !Word8  -- ^ Full 24-bit TrueColor.
                                    --   Possible range: [0, 255] each


-- | Classical 24-bit TrueColor with alpha channel 
-- in form of a RGBA tuple.
--
type Color = (Word8, Word8, Word8, Word8)


-- | Approximate 24-bit TrueColor to standard 16-color palette.
--
toStandard16Color :: Color -> TerminalColor
toStandard16Color color =
  let distances  = map (euclideanDistance color) standardColorMap
      numbered_d = zip [0..] distances
      colorIndex = fst $ minimumBy (compare `on` snd) numbered_d
  in Color16 colorIndex


-- |
--
toX256Term :: Color -> TerminalColor
toX256Term = undefined


-- |
--
toGrayscale :: Color -> TerminalColor
toGrayscale = undefined


-- |
--
toTrueColor :: Color -> TerminalColor
toTrueColor = undefined


-- | Evaluates Euclidean distance between two colors.
--
euclideanDistance :: Color -> Color -> Int
euclideanDistance (r1, g1, b1, _) (r2, g2, b2, _) =
  let r' = (r1 - r2) ^ 2
      g' = (g1 - g2) ^ 2
      b' = (b1 - b2) ^ 2
  in round . sqrt . fromIntegral $ r' + g' + b'


-- | Standard 16-color palette that is based on colors
-- from Windows Command Prompt. There are multiple
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#Colors variations> of the
-- standard 16-color palette, but we use Windows CMD as a reference.
--
standardColorMap :: [Color]
standardColorMap =
  [ ( 000, 000, 000, 000 ) -- Black
  , ( 128, 000, 000, 000 ) -- Red
  , ( 000, 128, 000, 000 ) -- Green
  , ( 128, 128, 000, 000 ) -- Yellow
  , ( 000, 000, 128, 000 ) -- Blue
  , ( 128, 000, 128, 000 ) -- Magenta
  , ( 000, 128, 128, 000 ) -- Cyan
  , ( 192, 192, 192, 000 ) -- White

  , ( 128, 128, 128, 000 ) -- Bright Black
  , ( 255, 000, 000, 000 ) -- Bright Red
  , ( 000, 255, 000, 000 ) -- Bright Green
  , ( 255, 255, 000, 000 ) -- Bright Yellow
  , ( 000, 000, 255, 000 ) -- Bright Blue
  , ( 255, 000, 255, 000 ) -- Bright Magenta
  , ( 000, 255, 255, 000 ) -- Bright Cyan
  , ( 255, 255, 255, 000 ) -- Bright White
  ]