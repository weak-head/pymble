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
      adviceSize Nothing Nothing
        `shouldBe` (80, 46)
    
    it "uses 0.58 font aspect ratio" $ do
      adviceSize (Just 100) Nothing
        `shouldBe` (100, 58)
      adviceSize Nothing (Just 58)
        `shouldBe` (100, 58)

  describe "adviceSizeWithRatio" $ do
    it "uses default width value (80)" $ do
      adviceSizeWithRatio 0.50 Nothing Nothing
        `shouldBe` (80, 40)

    it "advices based on custom ratio" $ do
      adviceSizeWithRatio 0.45 (Just 100) Nothing
        `shouldBe` (100, 45)
      adviceSizeWithRatio 0.67 Nothing (Just 67)
        `shouldBe` (100, 67)

    it "throws on zero or negative ratio" $ do
      evaluate (adviceSizeWithRatio 0 Nothing Nothing)
        `shouldThrow` errorCall "ratio should be greater than zero"
      evaluate (adviceSizeWithRatio (negate 0.25) Nothing Nothing)
        `shouldThrow` errorCall "ratio should be greater than zero"