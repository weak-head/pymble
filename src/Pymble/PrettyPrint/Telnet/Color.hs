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
    ) where

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


-- | Classical 32-bit color in form of a RGBA tuple.
--
type Color = (Word8, Word8, Word8, Word8)


-- |
--
toStandard16Color :: Color -> TerminalColor
toStandard16Color = undefined


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