{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
--
module Pymble.PrettyPrint.Telnet.Color
    (
    -- * Common types
      TerminalColor(..)
    , Color
    -- * Color approximation
    , toStandard16
    , toXterm256
    , toGrayscale
    , toTrueColor
    -- * Utility functions
    , bestMatchIx
    , euclideanDistance
    -- * Pre-defined color maps
    , winCmdColorMap
    , xterm256ColorMap
    ) where

import Data.Vector.Unboxed.Base (Unbox)
import Data.Vector.Unboxed.Deriving

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Word (Word8)
----------------------------------------------------------------------

-- | Extended <https://en.wikipedia.org/wiki/ANSI_escape_code#Colors ANSI compatible> terminal color.
--
data TerminalColor =
    Color16   !Word8                -- ^ Standard 16-color palette.
                                    --   Range: [0, 15].
  | Xterm256  !Word8                -- ^ Extended 256-color palette for x-term.
                                    --   Range: [0, 255].
  | Grayscale !Word8                -- ^ Grayscale subset of the extended 256-color palette
                                    --   for x-term. Range: [232, 255].
  | TrueColor !Word8 !Word8 !Word8  -- ^ Full 24-bit TrueColor (RGB).
                                    --   Range: [0, 255] each channel.


-- | 'Unbox', 'Vector', 'MVector' instances for 'TerminalColor'.
--
derivingUnbox "TerminalColor"
  [t| TerminalColor → (Int, Word8, Word8, Word8) |]

  [| \case
       Color16       c → (0, 0, 0, c)
       Xterm256      c → (1, 0, 0, c)
       Grayscale     c → (2, 0, 0, c)
       TrueColor r g b → (3, r, g, b) |]

  [| \(t, r, g, b) → case t of
        0 → Color16       b
        1 → Xterm256      b
        2 → Grayscale     b
        3 → TrueColor r g b |]


-- | Classical 24-bit TrueColor with alpha channel 
-- in form of a RGBA tuple.
--
type Color = (Word8, Word8, Word8, Word8)


-- | Approximate 24-bit TrueColor to standard 16-color palette.
-- 
-- The color approximation is done based on 'windowsCmdColorMap'.
-- There are multiple <https://en.wikipedia.org/wiki/ANSI_escape_code#Colors variations>
-- of the standard 16-color palette, but we use palette for Windows CMD as a reference
-- for the color approximation.
--
toStandard16 :: Color -> TerminalColor
toStandard16 = Color16 . fromIntegral . bestMatchIx winCmdColorMap


-- | Approximate 24-bit TrueColor to standard 
-- <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 256-color> xterm palette.
--
toXterm256 :: Color -> TerminalColor
toXterm256 = Xterm256 . fromIntegral . bestMatchIx xterm256ColorMap


-- | Approximate 24-bit TrueColor to grayscale subset of the
-- standard <https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit 256-color>
-- xterm palette.
--
toGrayscale :: Color -> TerminalColor
toGrayscale = Grayscale . offset . bestMatchIx xtermGrayscale
  where
    offset x       = fromIntegral $ 232 + x
    xtermGrayscale = drop 232 xterm256ColorMap


-- | Convert 24-bit TrueColor to Terminal TrueColor.
--
toTrueColor :: Color -> TerminalColor
toTrueColor (r, g, b, _) = TrueColor r g b


-- | Given the color and the color map
-- returns the index of the color from the map
-- that is the most similar one to the requested.
--
bestMatchIx :: [Color] -> Color -> Int
bestMatchIx colorMap color | null colorMap = error "empty color map"
bestMatchIx colorMap color =
  let distances  = map (euclideanDistance color) colorMap
      indexed_d  = zip [0..] distances
      colorIndex = fst $ minimumBy (compare `on` snd) indexed_d
  in colorIndex


-- | Measure the Euclidean distance between two colors in RGBA space.
-- Alpha channel is completely ignored during the measurement.
--
euclideanDistance :: Color -> Color -> Int
euclideanDistance (r1, g1, b1, _) (r2, g2, b2, _) =
  let r' = (fromIntegral r1 - fromIntegral r2) ^ 2
      g' = (fromIntegral g1 - fromIntegral g2) ^ 2
      b' = (fromIntegral b1 - fromIntegral b2) ^ 2
  in round . sqrt $ r' + g' + b'


-- | Standard 16-color palette for Windows Command Prompt.
--
winCmdColorMap :: [Color]
winCmdColorMap =
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


-- | Standard 256-color palette for xterm.
--
-- The palette has been generated using
-- <https://jonasjacek.github.io/colors/ the standard color data> as an input.
xterm256ColorMap :: [Color]
xterm256ColorMap =
  [ -- Standard colors 
    ( 000, 000, 000, 000 )
  , ( 128, 000, 000, 000 )
  , ( 000, 128, 000, 000 )
  , ( 128, 128, 000, 000 )
  , ( 000, 000, 128, 000 )
  , ( 128, 000, 128, 000 )
  , ( 000, 128, 128, 000 )
  , ( 192, 192, 192, 000 )

  -- High intensity colors
  , ( 128, 128, 128, 000 )
  , ( 255, 000, 000, 000 )
  , ( 000, 255, 000, 000 )
  , ( 255, 255, 000, 000 )
  , ( 000, 000, 255, 000 )
  , ( 255, 000, 255, 000 )
  , ( 000, 255, 255, 000 )
  , ( 255, 255, 255, 000 )

  -- Extended colors
  , ( 000, 000, 000, 000 )
  , ( 000, 000, 095, 000 )
  , ( 000, 000, 135, 000 )
  , ( 000, 000, 175, 000 )
  , ( 000, 000, 215, 000 )
  , ( 000, 000, 255, 000 )
  , ( 000, 095, 000, 000 )
  , ( 000, 095, 095, 000 )
  , ( 000, 095, 135, 000 )
  , ( 000, 095, 175, 000 )
  , ( 000, 095, 215, 000 )
  , ( 000, 095, 255, 000 )
  , ( 000, 135, 000, 000 )
  , ( 000, 135, 095, 000 )
  , ( 000, 135, 135, 000 )
  , ( 000, 135, 175, 000 )
  , ( 000, 135, 215, 000 )
  , ( 000, 135, 255, 000 )
  , ( 000, 175, 000, 000 )
  , ( 000, 175, 095, 000 )
  , ( 000, 175, 135, 000 )
  , ( 000, 175, 175, 000 )
  , ( 000, 175, 215, 000 )
  , ( 000, 175, 255, 000 )
  , ( 000, 215, 000, 000 )
  , ( 000, 215, 095, 000 )
  , ( 000, 215, 135, 000 )
  , ( 000, 215, 175, 000 )
  , ( 000, 215, 215, 000 )
  , ( 000, 215, 255, 000 )
  , ( 000, 255, 000, 000 )
  , ( 000, 255, 095, 000 )
  , ( 000, 255, 135, 000 )
  , ( 000, 255, 175, 000 )
  , ( 000, 255, 215, 000 )
  , ( 000, 255, 255, 000 )
  , ( 095, 000, 000, 000 )
  , ( 095, 000, 095, 000 )
  , ( 095, 000, 135, 000 )
  , ( 095, 000, 175, 000 )
  , ( 095, 000, 215, 000 )
  , ( 095, 000, 255, 000 )
  , ( 095, 095, 000, 000 )
  , ( 095, 095, 095, 000 )
  , ( 095, 095, 135, 000 )
  , ( 095, 095, 175, 000 )
  , ( 095, 095, 215, 000 )
  , ( 095, 095, 255, 000 )
  , ( 095, 135, 000, 000 )
  , ( 095, 135, 095, 000 )
  , ( 095, 135, 135, 000 )
  , ( 095, 135, 175, 000 )
  , ( 095, 135, 215, 000 )
  , ( 095, 135, 255, 000 )
  , ( 095, 175, 000, 000 )
  , ( 095, 175, 095, 000 )
  , ( 095, 175, 135, 000 )
  , ( 095, 175, 175, 000 )
  , ( 095, 175, 215, 000 )
  , ( 095, 175, 255, 000 )
  , ( 095, 215, 000, 000 )
  , ( 095, 215, 095, 000 )
  , ( 095, 215, 135, 000 )
  , ( 095, 215, 175, 000 )
  , ( 095, 215, 215, 000 )
  , ( 095, 215, 255, 000 )
  , ( 095, 255, 000, 000 )
  , ( 095, 255, 095, 000 )
  , ( 095, 255, 135, 000 )
  , ( 095, 255, 175, 000 )
  , ( 095, 255, 215, 000 )
  , ( 095, 255, 255, 000 )
  , ( 135, 000, 000, 000 )
  , ( 135, 000, 095, 000 )
  , ( 135, 000, 135, 000 )
  , ( 135, 000, 175, 000 )
  , ( 135, 000, 215, 000 )
  , ( 135, 000, 255, 000 )
  , ( 135, 095, 000, 000 )
  , ( 135, 095, 095, 000 )
  , ( 135, 095, 135, 000 )
  , ( 135, 095, 175, 000 )
  , ( 135, 095, 215, 000 )
  , ( 135, 095, 255, 000 )
  , ( 135, 135, 000, 000 )
  , ( 135, 135, 095, 000 )
  , ( 135, 135, 135, 000 )
  , ( 135, 135, 175, 000 )
  , ( 135, 135, 215, 000 )
  , ( 135, 135, 255, 000 )
  , ( 135, 175, 000, 000 )
  , ( 135, 175, 095, 000 )
  , ( 135, 175, 135, 000 )
  , ( 135, 175, 175, 000 )
  , ( 135, 175, 215, 000 )
  , ( 135, 175, 255, 000 )
  , ( 135, 215, 000, 000 )
  , ( 135, 215, 095, 000 )
  , ( 135, 215, 135, 000 )
  , ( 135, 215, 175, 000 )
  , ( 135, 215, 215, 000 )
  , ( 135, 215, 255, 000 )
  , ( 135, 255, 000, 000 )
  , ( 135, 255, 095, 000 )
  , ( 135, 255, 135, 000 )
  , ( 135, 255, 175, 000 )
  , ( 135, 255, 215, 000 )
  , ( 135, 255, 255, 000 )
  , ( 175, 000, 000, 000 )
  , ( 175, 000, 095, 000 )
  , ( 175, 000, 135, 000 )
  , ( 175, 000, 175, 000 )
  , ( 175, 000, 215, 000 )
  , ( 175, 000, 255, 000 )
  , ( 175, 095, 000, 000 )
  , ( 175, 095, 095, 000 )
  , ( 175, 095, 135, 000 )
  , ( 175, 095, 175, 000 )
  , ( 175, 095, 215, 000 )
  , ( 175, 095, 255, 000 )
  , ( 175, 135, 000, 000 )
  , ( 175, 135, 095, 000 )
  , ( 175, 135, 135, 000 )
  , ( 175, 135, 175, 000 )
  , ( 175, 135, 215, 000 )
  , ( 175, 135, 255, 000 )
  , ( 175, 175, 000, 000 )
  , ( 175, 175, 095, 000 )
  , ( 175, 175, 135, 000 )
  , ( 175, 175, 175, 000 )
  , ( 175, 175, 215, 000 )
  , ( 175, 175, 255, 000 )
  , ( 175, 215, 000, 000 )
  , ( 175, 215, 095, 000 )
  , ( 175, 215, 135, 000 )
  , ( 175, 215, 175, 000 )
  , ( 175, 215, 215, 000 )
  , ( 175, 215, 255, 000 )
  , ( 175, 255, 000, 000 )
  , ( 175, 255, 095, 000 )
  , ( 175, 255, 135, 000 )
  , ( 175, 255, 175, 000 )
  , ( 175, 255, 215, 000 )
  , ( 175, 255, 255, 000 )
  , ( 215, 000, 000, 000 )
  , ( 215, 000, 095, 000 )
  , ( 215, 000, 135, 000 )
  , ( 215, 000, 175, 000 )
  , ( 215, 000, 215, 000 )
  , ( 215, 000, 255, 000 )
  , ( 215, 095, 000, 000 )
  , ( 215, 095, 095, 000 )
  , ( 215, 095, 135, 000 )
  , ( 215, 095, 175, 000 )
  , ( 215, 095, 215, 000 )
  , ( 215, 095, 255, 000 )
  , ( 215, 135, 000, 000 )
  , ( 215, 135, 095, 000 )
  , ( 215, 135, 135, 000 )
  , ( 215, 135, 175, 000 )
  , ( 215, 135, 215, 000 )
  , ( 215, 135, 255, 000 )
  , ( 215, 175, 000, 000 )
  , ( 215, 175, 095, 000 )
  , ( 215, 175, 135, 000 )
  , ( 215, 175, 175, 000 )
  , ( 215, 175, 215, 000 )
  , ( 215, 175, 255, 000 )
  , ( 215, 215, 000, 000 )
  , ( 215, 215, 095, 000 )
  , ( 215, 215, 135, 000 )
  , ( 215, 215, 175, 000 )
  , ( 215, 215, 215, 000 )
  , ( 215, 215, 255, 000 )
  , ( 215, 255, 000, 000 )
  , ( 215, 255, 095, 000 )
  , ( 215, 255, 135, 000 )
  , ( 215, 255, 175, 000 )
  , ( 215, 255, 215, 000 )
  , ( 215, 255, 255, 000 )
  , ( 255, 000, 000, 000 )
  , ( 255, 000, 095, 000 )
  , ( 255, 000, 135, 000 )
  , ( 255, 000, 175, 000 )
  , ( 255, 000, 215, 000 )
  , ( 255, 000, 255, 000 )
  , ( 255, 095, 000, 000 )
  , ( 255, 095, 095, 000 )
  , ( 255, 095, 135, 000 )
  , ( 255, 095, 175, 000 )
  , ( 255, 095, 215, 000 )
  , ( 255, 095, 255, 000 )
  , ( 255, 135, 000, 000 )
  , ( 255, 135, 095, 000 )
  , ( 255, 135, 135, 000 )
  , ( 255, 135, 175, 000 )
  , ( 255, 135, 215, 000 )
  , ( 255, 135, 255, 000 )
  , ( 255, 175, 000, 000 )
  , ( 255, 175, 095, 000 )
  , ( 255, 175, 135, 000 )
  , ( 255, 175, 175, 000 )
  , ( 255, 175, 215, 000 )
  , ( 255, 175, 255, 000 )
  , ( 255, 215, 000, 000 )
  , ( 255, 215, 095, 000 )
  , ( 255, 215, 135, 000 )
  , ( 255, 215, 175, 000 )
  , ( 255, 215, 215, 000 )
  , ( 255, 215, 255, 000 )
  , ( 255, 255, 000, 000 )
  , ( 255, 255, 095, 000 )
  , ( 255, 255, 135, 000 )
  , ( 255, 255, 175, 000 )
  , ( 255, 255, 215, 000 )
  , ( 255, 255, 255, 000 )

  -- Grayscale from black to white in 24 steps
  , ( 008, 008, 008, 000 )
  , ( 018, 018, 018, 000 )
  , ( 028, 028, 028, 000 )
  , ( 038, 038, 038, 000 )
  , ( 048, 048, 048, 000 )
  , ( 058, 058, 058, 000 )
  , ( 068, 068, 068, 000 )
  , ( 078, 078, 078, 000 )
  , ( 088, 088, 088, 000 )
  , ( 096, 096, 096, 000 )
  , ( 102, 102, 102, 000 )
  , ( 118, 118, 118, 000 )
  , ( 128, 128, 128, 000 )
  , ( 138, 138, 138, 000 )
  , ( 148, 148, 148, 000 )
  , ( 158, 158, 158, 000 )
  , ( 168, 168, 168, 000 )
  , ( 178, 178, 178, 000 )
  , ( 188, 188, 188, 000 )
  , ( 198, 198, 198, 000 )
  , ( 208, 208, 208, 000 )
  , ( 218, 218, 218, 000 )
  , ( 228, 228, 228, 000 )
  , ( 238, 238, 238, 000 )
  ]
