{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- |
--
module Pymble.PrettyPrint.Telnet
    (
    -- * Character and color encoding
      encodeColoredChar
    , encodeTerminalColor
    , encodeChar
    , encodeReset

    ) where

import Data.Array.Repa         as R

import Pymble.Image.Convert
import Pymble.PrettyPrint.Telnet.Color
----------------------------------------------------------------------


-- | Encodes character to be rendered with the specified 'TerminalColor'.
--
encodeColoredChar :: TerminalColor -> Char -> ShowS
encodeColoredChar color char =
  encodeTerminalColor color . encodeChar char . encodeReset


-- | Encodes 'TerminalColor' into escape sequence. The text
-- following the sequence would have the specified
-- foreground color.
--
encodeTerminalColor :: TerminalColor -> ShowS
encodeTerminalColor = \case
    Color16 c       -> ss "\ESC["      . shows (toC c) . ss "m"
    Xterm256 c      -> ss "\ESC[38;5;" . shows c       . ss "m"
    Grayscale c     -> ss "\ESC[38;5;" . shows c       . ss "m"
    TrueColor r g b -> ss "\ESC[38;2;" . shows r . ss ";" .
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