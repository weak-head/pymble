{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- |
--
module Pymble.PrettyPrint.Telnet
    (
    -- * ASCII art pretty print
      ColoredTermChar
    , ColorScheme(..)
    , evalAsTerminalColor
    , prettyPrint
    -- * Character and color encoding
    , encodeColoredChar
    , encodeTerminalColor
    , encodeChar
    , encodeReset
    -- * Terminal control sequences
    , termClear
    ) where

import Data.Array.Repa as R
import Data.Bool (bool)

import           Pymble.Image.Convert
import qualified Pymble.PrettyPrint.Telnet.Color as TC
----------------------------------------------------------------------


-- | Color scheme that is used during ASCII art pretty print.
--
data ColorScheme = Color16 | Xterm256 | Grayscale | TrueColor
  deriving (Read, Show, Eq, Ord)


-- | The tuple represents char that should be rendered
-- using the specified terminal color.
--
type ColoredTermChar = (Char, TC.TerminalColor)


-- | Convert delayed array of 'ColoredChar' to
-- fully evaluated unboxed array of 'ColoredTermChar'.
--
evalAsTerminalColor :: ColorScheme 
                    -> DelayedArray ColoredChar
                    -> IO (UnboxedArray ColoredTermChar)
evalAsTerminalColor colorScheme = R.computeP . R.map (fmap converter)
  where
    converter = case colorScheme of
      Color16   -> TC.toStandard16
      Xterm256  -> TC.toXterm256
      Grayscale -> TC.toGrayscale
      TrueColor -> TC.toTrueColor


-- | Pretty print the fully evaluated unboxed array of 'ColoredTermChar'
-- to the terminal-encoded string representation.
--
prettyPrint :: UnboxedArray ColoredTermChar -> ShowS
prettyPrint arr =
    let Z :. width :. height = R.extent arr
        coords = [(w, h) | h <- [0 .. height - 1]
                         , w <- [0 .. width  - 1]]
    in foldl (.) id $ (convert width) <$> coords 
  where
    -- Converts the colored char
    -- to the terminal-encoded representation
    convert maxWidth (w, h) =
      let (ch, tc) = arr ! (Z :. w :. h)
          newl     = bool id (showString "\n") (w == maxWidth - 1)
      in (encodeColoredChar tc ch) . newl


-- | Encodes character to be rendered with the specified 'TerminalColor'.
--
encodeColoredChar :: TC.TerminalColor -> Char -> ShowS
encodeColoredChar color char =
  encodeTerminalColor color . encodeChar char . encodeReset


-- | Encodes 'TerminalColor' into escape sequence. The text
-- following the sequence would have the specified
-- foreground color.
--
encodeTerminalColor :: TC.TerminalColor -> ShowS
encodeTerminalColor = \case
    TC.Color16 c       -> ss "\ESC["      . shows (toC c) . ss "m"
    TC.Xterm256 c      -> ss "\ESC[38;5;" . shows c       . ss "m"
    TC.Grayscale c     -> ss "\ESC[38;5;" . shows c       . ss "m"
    TC.TrueColor r g b -> ss "\ESC[38;2;" . shows r . ss ";" .
                                         shows g . ss ";" .
                                         shows b . ss "m"
  where
    -- adjusts [0-15] standard color to the number that is
    -- expected by the terminal for colored output
    -- https://en.wikipedia.org/wiki/ANSI_escape_code#SGR
    toC x = if x < 8 then x + 30 else x + (90 - 8)

    -- just a shortcut for less cluttering
    ss = showString


-- | Encodes character to be rendered as-is.
--
encodeChar :: Char -> ShowS
encodeChar = showChar


-- | Reset all attributes off.
--
encodeReset :: ShowS
encodeReset = showString "\ESC[0;00m"


-- | Clear terminal.
--
termClear :: ShowS
termClear = showString "\ESC[2J"