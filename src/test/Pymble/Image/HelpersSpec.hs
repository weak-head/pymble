{-# LANGUAGE OverloadedStrings #-}

module Pymble.Image.HelpersSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Exception (evaluate)

import Pymble.Image.Helpers
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "adviceSize" $ do
    it "has default value" $ do
      adviceSize (200, 200) Nothing Nothing
        `shouldBe` (80, 46)
    
    it "uses 0.58 font aspect ratio" $ do
      adviceSize (300, 300) (Just 100) Nothing
        `shouldBe` (100, 58)
      adviceSize (400, 400) Nothing (Just 58)
        `shouldBe` (100, 58)

    it "adjusts art size based on the actual image size" $ do
      adviceSize (300, 150) (Just 100) Nothing
        `shouldBe` (100, 29)
      adviceSize (150, 300) (Just 100) Nothing
        `shouldBe` (100, 116)

    it "returns width and height if explicitly specified" $ do
      adviceSize (300, 150) (Just 20) (Just 180)
        `shouldBe` (300, 87)
      adviceSize (200, 200) (Just 40) (Just 40)
        `shouldBe` (40, 40)

  describe "adviceSizeWithRatio" $ do
    it "uses default width value (80)" $ do
      adviceSizeWithRatio 0.50 (120, 120) Nothing Nothing
        `shouldBe` (80, 40)

    it "advices based on custom ratio" $ do
      adviceSizeWithRatio 0.45 (120, 120) (Just 100) Nothing
        `shouldBe` (100, 45)
      adviceSizeWithRatio 0.67 (150, 150) Nothing (Just 67)
        `shouldBe` (100, 67)

    it "adjusts art size based on the actual image size" $ do
      adviceSizeWithRatio 0.50 (120, 60) (Just 100) Nothing
        `shouldBe` (100, 25)
      adviceSizeWithRatio 0.67 (50, 150) Nothing (Just 67)
        `shouldBe` (33, 67)

    it "returns width and height if explicitly specified" $ do
      adviceSizeWithRatio 0.22 (300, 150) (Just 20) (Just 180)
        `shouldBe` (300, 33)
      adviceSizeWithRatio 0.87 (200, 200) (Just 40) (Just 40)
        `shouldBe` (40, 40)

    it "throws on zero or negative ratio" $ do
      evaluate (adviceSizeWithRatio 0 (100, 100) Nothing Nothing)
        `shouldThrow` errorCall "ratio should be greater than zero"
      evaluate (adviceSizeWithRatio (negate 0.25) (150, 150) Nothing Nothing)
        `shouldThrow` errorCall "ratio should be greater than zero"
    
    it "throws on zero or negative image size" $ do
      evaluate (adviceSizeWithRatio 0.58 (0, 100) Nothing Nothing)
        `shouldThrow` errorCall "image width should be greater than zero"
      evaluate (adviceSizeWithRatio 0.58 (negate 10, 100) Nothing Nothing)
        `shouldThrow` errorCall "image width should be greater than zero"
      evaluate (adviceSizeWithRatio 0.58 (100, 0) Nothing Nothing)
        `shouldThrow` errorCall "image height should be greater than zero"
      evaluate (adviceSizeWithRatio 0.58 (100, negate 10) Nothing Nothing)
        `shouldThrow` errorCall "image height should be greater than zero"