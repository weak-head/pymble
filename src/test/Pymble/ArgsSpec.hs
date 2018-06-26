{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.ArgsSpec where

import Test.Hspec
import Test.QuickCheck

import Pymble.Args
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  describe "ArgsSpec" $ do
    it "works" $ do
      1 `shouldBe` 1