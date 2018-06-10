{-# LANGUAGE OverloadedStrings #-}

module Pymble.Image.ConvertSpec where

import Test.Hspec
import Pymble.Image.Convert
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec =

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

