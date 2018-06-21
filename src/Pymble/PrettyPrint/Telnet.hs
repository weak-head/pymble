{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- |
--
module Pymble.PrettyPrint.Telnet
    (

    ) where

import Data.Array.Repa         as R

import Pymble.Image.Convert
import Pymble.PrettyPrint.Telnet.Color
----------------------------------------------------------------------

-- |
--
encodeColoredChar :: ColoredChar -> ShowS
encodeColoredChar (char, color) = undefined


-- |
--
encodeTerminalColor :: TerminalColor -> ShowS
encodeTerminalColor = \case
    Color16 c       -> showString "\ESC[" . shows (adj c) . end
    Xterm256 c      -> showString "\ESC[38;5;" . shows c . end
    Grayscale c     -> showString "\ESC[38;5;" . shows c . end
    TrueColor r g b -> showString "\ESC[38;2;" .
                          shows r . showString ";" .
                          shows g . showString ";" .
                          shows b . end
  where
    adj x = if x < 8 then x + 30 else x + 90 
    end = showString "m"

-- |
--
encodeCharacter :: Char -> ShowS
encodeCharacter = showChar