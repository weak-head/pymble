{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- This module exposes font specifications that from the conversion
-- point of view are basically brightness maps.
--
-- The fontspecs / brightness maps
-- are used to find the best matching substitution of the
-- image area and convert the image or graphics into colored ASCII art
-- that is as close to the original image as possible.
--
module Pymble.Image.Fontspec
    (
    -- * Pre-generated font specifications
      courierFull
    ) where

import           Data.Tuple (swap)
import qualified Data.Map.Lazy as ML
import           Data.Word
----------------------------------------------------------------------

-- | The brightness map of the
-- <https://en.wikipedia.org/wiki/Courier_(typeface) Courier New> font
-- with the full standard ASCII char set.
--
-- This font specification has been generated using
-- the fontspec/fontspec.py script:
--
-- > $ python fontspec.py \
-- >     -d "`{}[]|~!@#$%^&*()_+-=1234567890AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz<>,./\\\";:\'"
-- >     --distinct-brightness
--
courierFull :: ML.Map Word8 Char
courierFull = ML.fromList $ map swap
  [ ('`', 0)
  , ('.', 23)
  , ('~', 36)
  , ('^', 44)
  , ('-', 46)
  , ('_', 47)
  , ('\'', 51)
  , (',', 55)
  , ('|', 63)
  , (':', 69)
  , ('/', 73)
  , ('\\', 74)
  , ('!', 75)
  , ('*', 81)
  , ('{', 82)
  , ('}', 84)
  , (';', 86)
  , ('[', 92)
  , (']', 93)
  , (')', 94)
  , ('(', 95)
  , ('7', 97)
  , ('i', 105)
  , ('1', 109)
  , ('r', 117)
  , ('c', 119)
  , ('v', 120)
  , ('I', 121)
  , ('j', 124)
  , ('z', 131)
  , ('o', 132)
  , ('%', 133)
  , ('J', 134)
  , ('u', 135)
  , ('3', 141)
  , ('C', 142)
  , ('n', 143)
  , ('L', 147)
  , ('&', 148)
  , ('5', 150)
  , ('f', 151)
  , ('V', 152)
  , ('a', 161)
  , ('4', 162)
  , ('0', 164)
  , ('e', 167)
  , ('$', 170)
  , ('6', 171)
  , ('9', 172)
  , ('P', 175)
  , ('S', 176)
  , ('F', 177)
  , ('D', 181)
  , ('G', 182)
  , ('8', 192)
  , ('X', 193)
  , ('d', 197)
  , ('b', 199)
  , ('m', 201)
  , ('A', 202)
  , ('g', 204)
  , ('K', 206)
  , ('H', 208)
  , ('p', 209)
  , ('q', 210)
  , ('@', 212)
  , ('E', 214)
  , ('#', 216)
  , ('N', 226)
  , ('B', 227)
  , ('Q', 228)
  , ('W', 248)
  , ('M', 255)
  ]