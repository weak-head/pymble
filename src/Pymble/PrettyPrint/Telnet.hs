{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- |
--
module Pymble.PrettyPrint.Telnet
    (
    -- *
      ColoredTermChar
    , convert
    , prettyPrint
    -- * Character and color encoding
    , encodeColoredChar
    , encodeTerminalColor
    , encodeChar
    , encodeReset
    -- * Terminal control sequences
    , termClear
    ) where

import qualified Data.Array.Repa as R
import           Data.Tuple (swap)

import Pymble.Image.Convert
import qualified Pymble.PrettyPrint.Telnet.Color as TC
----------------------------------------------------------------------


-- |
--
data ColorScheme =
    Color16
  | Xterm256
  | Grayscale
  | TrueColor


-- |
--
type ColoredTermChar = (Char, TC.TerminalColor)


-- |
--
-- convert :: (ColoredChar -> ColoredTermChar)
--               -> DelayedArray ColoredChar
--               -> DelayedArray ColoredTermChar
-- convert = R.map


convert :: DelayedArray ColoredTermChar -> DelayedArray String
convert = R.map $ \c -> (uncurry $ flip $ encodeColoredChar) c ""


-- | Pretty prints the delayed array of 'ColoredTermChar' to
-- the delayed array of terminal-encoded strings.
--
prettyPrint :: ColorScheme -> DelayedArray ColoredChar -> IO String
prettyPrint colorScheme array = do
    let coloredArray = R.map (fmap converter) array
        converted    = convert coloredArray
    -- R.foldAllP (++) [] converted
    undefined
  where
    converter = case colorScheme of
      Color16   -> TC.toStandard16
      Xterm256  -> TC.toXterm256
      Grayscale -> TC.toGrayscale
      TrueColor -> TC.toTrueColor


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