{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.Telnet.Api.ParserSpec where

import Test.Hspec
import Test.QuickCheck

import Pymble.Telnet.Api.Parser

----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "helpParser" $ do
    it "fails on empty string" $ do
      parseCommand ""
        `shouldBe` failure 

    it "ignores whitespace on both sides" $ do
      parseCommand " \t  help \t  "
        `shouldBe` cmd Help

    it "ignores additional input" $ do
      parseCommand "  help   some additional text 123  "
        `shouldBe` cmd Help
    
    it "parses strict help" $ do
      parseCommand "help"
        `shouldBe` cmd Help


------------------------------

failure = Left "failed to parse"
cmd    = Right 