{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.Telnet.Api.ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Prelude hiding (fail)

import Text.Megaparsec

import Pymble.Telnet.Api.Parser

----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "helpParser" $ do
    it "fails on empty string" $ do
      parseWith helpParser ""
        `shouldBe` fail 

    it "ignores whitespace on both sides" $ do
      parseWith helpParser " \t  help \t  "
        `shouldBe` cmd Help

    it "ignores additional input" $ do
      parseWith helpParser "  help   some additional text 123  "
        `shouldBe` cmd Help
    
    it "parses exact string" $ do
      parseWith helpParser "help"
        `shouldBe` cmd Help

    it "case insensitive" $ do
      parseWith helpParser "  HeLP  "
        `shouldBe` cmd Help

  describe "quitParser" $ do
    it "fails on empty string" $ do
      parseWith quitParser ""
        `shouldBe` fail

    it "ignores whitespace on both sides" $ do
      parseWith quitParser "   quit   "
        `shouldBe` cmd Quit
      parseWith quitParser " q     \t \t"
        `shouldBe` cmd Quit
      parseWith quitParser " \t  exit  "
        `shouldBe` cmd Quit

    it "fails on additional input" $ do
      parseWith quitParser " quit abc"
        `shouldBe` fail
      parseWith quitParser " q some 123"
        `shouldBe` fail
      parseWith quitParser " exit text"
        `shouldBe` fail

    it "parses exact string" $ do
      parseWith quitParser "quit"
        `shouldBe` cmd Quit
      parseWith quitParser "q"
        `shouldBe` cmd Quit
      parseWith quitParser "exit"
        `shouldBe` cmd Quit

    it "case insensitive" $ do
      parseWith quitParser "  QUIT "
        `shouldBe` cmd Quit
      parseWith quitParser " ExIt  "
        `shouldBe` cmd Quit


------------------------------


parseWith :: Parser a -> String -> Either ParsingError a
parseWith parser str = case (parse parser "" str) of
  Left err -> fail
  Right xs -> Right xs

fail = Left "failed to parse"
cmd  = Right 