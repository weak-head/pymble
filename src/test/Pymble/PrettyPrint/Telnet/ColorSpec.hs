{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.PrettyPrint.Telnet.ColorSpec where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Word
import qualified Data.Set as S
import Test.Hspec
import Test.QuickCheck

import Pymble.PrettyPrint.Telnet.Color 
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "euclideanDistance" $ do
    it "for identical colors is zero" $ do
      let color = (100, 200, 250, 0)
      euclideanDistance color color `shouldBe` 0

    it "ignores alpha" $ do
      let c1 = (100, 200, 100, 50)
          c2 = (100, 200, 100, 25)
      euclideanDistance c1 c2 `shouldBe` 0

    it "measures rounded distance" $ do
      let c1 = (10, 10, 10, 0)
          c2 = (20, 20, 20, 0)
      euclideanDistance c1 c2 `shouldBe` 17
    
    it "rounds distance to nearest integral" $ do
      let c1 = (50, 50, 50, 0)
          c2 = (75, 75, 75, 0)
          c3 = (90, 75, 90, 0)
      euclideanDistance c1 c2 `shouldBe` 43  -- 43.30 -> 43.0
      euclideanDistance c1 c3 `shouldBe` 62  -- 61.84 -> 62.0

    it "can handle edge cases" $ do
      let c1 = (0,     0,   0, 0)
          c2 = (255, 255, 255, 0)
      euclideanDistance c1 c2 `shouldBe` 442

  describe "bestMatchIx" $ do
    it "with empty color map throws" $ do
      evaluate(bestMatchIx [] (0, 0, 0, 0)) `shouldThrow` errorCall "empty color map"

    it "with singleton map acts as const" $ do
      property @((Word8, Word8, Word8, Word8) -> Bool) $
        \color -> bestMatchIx [(50, 50, 50, 0)] color == 0

    it "returns index in bounds of the color map" $ do
      let colorMap = [ (0, 0, 0, 0)
                     , (55, 55, 55, 0)
                     , (127, 127, 127, 0)
                     , (211, 211, 211, 211)
                     , (255, 255, 255, 255) ]
          bounds xs i
            | i < 0          = False
            | i >= length xs = False
            | otherwise      = True
      property @((Word8, Word8, Word8, Word8) -> Bool) $
        \color -> bounds colorMap (bestMatchIx colorMap color)

    it "returns index of the best match" $ do
      let colorMap = [ (  0,   0,   0, 0)
                     , ( 50,  50,  50, 0)
                     , (100, 100, 100, 0)
                     , (125, 125, 125, 0)
                     , (175, 175, 175, 0)
                     , (255, 255, 255, 0)
                     ]
          scenarios = [ ((  0,   0,   0, 0), 0)
                      , (( 15,  15,  15, 0), 0)
                      , (( 35,  35,  35, 0), 1)
                      , (( 65,  65,  65, 0), 1)
                      , ((110, 110, 110, 0), 2)
                      , ((115, 115, 115, 0), 3)
                      , ((135, 135, 135, 0), 3)
                      , ((185, 185, 185, 0), 4)
                      , ((200, 200, 200, 0), 4)
                      , ((210, 210, 210, 0), 4)
                      , ((225, 225, 225, 0), 5)
                      , ((255, 255, 255, 0), 5)
                      ]
      forM_ scenarios $ \(color, expectation) ->
        bestMatchIx colorMap color `shouldBe` expectation

  describe "toStandard16" $ do
    it "returns only standard colors" $ do
      let isStandard (Color16 c)
            | c < 0     = False
            | c > 15    = False
            | otherwise = True
      property @((Word8, Word8, Word8, Word8) -> Bool) $
        \color -> isStandard $ toStandard16 color

  describe "toXterm256" $ do
    it "returns only xterm colors" $ do
      let isXterm (Xterm256 _) = True
      property @((Word8, Word8, Word8, Word8) -> Bool) $
        \color -> isXterm $ toXterm256 color

  describe "toGrayscale" $ do
    it "returns only grayscale subset of xterm colors" $ do
      let isGrayscale (Grayscale c)
            | c < 232   = False
            | c > 255   = False
            | otherwise = True
      property @((Word8, Word8, Word8, Word8) -> Bool) $
        \color -> isGrayscale $ toGrayscale color 

  describe "toTrueColor" $ do
    it "returns only true colors" $ do
      let isTrueColor (TrueColor _ _ _) = True
      property @((Word8, Word8, Word8, Word8) -> Bool) $
        \color -> isTrueColor $ toTrueColor color

    it "returns unchanged RGB color" $ do
      let areSame (r, g, b, _) (TrueColor r' g' b') =
           (r == r') && (g == g') && (b == b')
      property @((Word8, Word8, Word8, Word8) -> Bool) $
        \color -> areSame color $ toTrueColor color

  describe "winCmdColorMap" $ do
    it "has no duplicates" $ do
      hasDuplicates winCmdColorMap
        `shouldBe` False

    it "contains 16 colors" $ do
      length winCmdColorMap
        `shouldBe` 16

    -- this explicit indexing trick allows to track code coverage
    it "has zero alpha for every color" $ do
      forM_ [0 .. 15] $ \index ->
        (\(_, _, _, a) -> a `shouldBe` 0) (winCmdColorMap !! index) 

  describe "xterm256ColorMap" $ do
    it "has no duplicates" $ do
      -- Standard 16 color map intersects
      -- with the extended colors
      hasDuplicates (drop 16 xterm256ColorMap)
        `shouldBe` False
      hasDuplicates (take 16 xterm256ColorMap)
        `shouldBe` False

    it "contains 256 colors" $ do
      length xterm256ColorMap
        `shouldBe` 256

    -- this explicit indexing trick allows to track code coverage
    it "has zero alpha for every color" $ do
      forM_ [0 .. 255] $ \index ->
        (\(_, _, _, a) -> a `shouldBe` 0) (xterm256ColorMap !! index)


--- helpers ----------------------------

hasDuplicatesWith :: Ord a => S.Set a -> [a] -> Bool
hasDuplicatesWith known [] = False
hasDuplicatesWith known (x:xs)
  | S.member x known = True
  | otherwise        = hasDuplicatesWith (S.insert x known) xs

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates = hasDuplicatesWith S.empty