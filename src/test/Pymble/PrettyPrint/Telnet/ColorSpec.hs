{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.PrettyPrint.Telnet.ColorSpec where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Word
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

  describe "toGrayscale" $ do
    it "returns only grayscale xterm subset" $ do
      let isGrayscale (Grayscale c)
            | c < 232   = False
            | c > 255   = False
            | otherwise = True
      property @((Word8, Word8, Word8, Word8) -> Bool) $
        \color -> isGrayscale $ toGrayscale color 