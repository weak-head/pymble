{-# LANGUAGE OverloadedStrings #-}

module Pymble.Image.ConvertSpec where

import Test.Hspec
import Pymble.Image.Convert
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "unifyColor" $ do
    it "empty list" $ do
        unifyColor [] `shouldBe` (0, 0, 0, 0)

    it "single item" $ do
        unifyColor [(172, 233, 127, 255)] `shouldBe` (172, 233, 127, 255)

    it "replaces alpha" $ do
        unifyColor [(1, 2, 3, 4)] `shouldBe` (1, 2, 3, 255)

    it "computes average" $ do
        unifyColor [ (100, 100, 100, 100)
                   , (125, 125, 125, 125)
                   , (200, 200, 200, 200)
                   , (75,  75,  75,  75 )] `shouldBe` (125, 125, 125, 255)

  describe "averageBrightness" $ do
    it "empty list" $ do
        averageBrightness [] `shouldBe` 0

    it "single item" $ do
        averageBrightness [(100, 150, 200, 125)] `shouldBe` 150

    it "ignores alpha" $ do
        averageBrightness [(255, 255, 255, 0)] `shouldBe` 255

    it "computes average" $ do
        averageBrightness [ (100, 150, 200, 200)
                          , (50,  75,  100, 200)
                          , (75,  75,  75,  75)
                          , (75,  100, 125, 200)] `shouldBe` 100

