{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.Image.ConvertSpec where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Map.Lazy (fromList, empty)
import Data.Word
import Test.Hspec
import Test.QuickCheck

import Pymble.Image.Convert
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "unifyColor" $ do
    it "for empty list is zero" $ do
      unifyColor [] `shouldBe` (0, 0, 0, 0)

    it "for single item is the item" $ do
      unifyColor [(172, 233, 127, 255)] `shouldBe` (172, 233, 127, 255)

    it "always replaces alpha" $ do
      unifyColor [(1, 2, 3, 4)] `shouldBe` (1, 2, 3, 255)

    it "computes average" $ do
      unifyColor [ (100, 100, 100, 100)
                 , (125, 125, 125, 125)
                 , (200, 200, 200, 200)
                 , (75,  75,  75,  75 )] `shouldBe` (125, 125, 125, 255)

  describe "averageBrightness" $ do
    it "for empty list is zero" $ do
      averageBrightness [] `shouldBe` 0

    it "for single item is item average" $ do
      averageBrightness [(100, 150, 200, 125)] `shouldBe` 150

    it "always ignores alpha" $ do
      averageBrightness [(255, 255, 255, 0)] `shouldBe` 255

    it "computes average" $ do
      averageBrightness [ (100, 150, 200, 200)
                          , (50,  75,  100, 200)
                          , (75,  75,  75,  75)
                          , (75,  100, 125, 200)] `shouldBe` 100

  describe "bestFit" $ do
    it "with empty map throws" $ do
      evaluate(bestFit empty 100) `shouldThrow` errorCall "empty brightness map"

    it "with singleton map acts as const" $ do
      property @(Word8 -> Bool) $
        \bright -> bestFit (fromList [(0, '#')]) bright == '#'

    it "returns best fit" $ do
      let bmap = fromList [ ( 10, '#')
                          , ( 20, '.')
                          , ( 50, '?')
                          , ( 70, 'A')
                          , (150, 'B')
                          , (220, 'C')
                          , (255, 'D') ]
          scenarios = [ (  0, '#')
                      , ( 10, '#')
                      , ( 14, '#')
                      , ( 16, '.')
                      , ( 25, '.')
                      , ( 34, '.')
                      , ( 36, '?')
                      , ( 51, '?')
                      , (100, 'A')
                      , (120, 'B')
                      , (160, 'B')
                      , (190, 'C')
                      , (220, 'C')
                      , (240, 'D')
                      , (255, 'D') ]
      forM_ scenarios $ \(brightness, expectation) ->
        bestFit bmap brightness `shouldBe` expectation