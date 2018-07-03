{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.Telnet.Api.ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Prelude hiding (fail)

import Data.Semigroup
import Text.Megaparsec

import Pymble.Telnet.Api.Parser
import Pymble.PrettyPrint.Terminal
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "helpParser" $ do
    it "fails on empty string" $ do
      parseWith helpParser ""
        `shouldBe` fail 

    it "fails on partial or redundant string" $ do
      parseWith helpParser "hel"
        `shouldBe` fail
      parseWith helpParser "helpp"
        `shouldBe` fail
      parseWith helpParser "help123"
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


  describe "viewConfigParser" $ do
    it "fails on empty string" $ do
      parseWith viewConfigParser ""
        `shouldBe` fail 

    it "fails on partial or redundant string" $ do
      parseWith viewConfigParser "conf"
        `shouldBe` fail
      parseWith viewConfigParser "configg"
        `shouldBe` fail
      parseWith viewConfigParser "config123"
        `shouldBe` fail

    it "ignores whitespace on both sides" $ do
      parseWith viewConfigParser " \t  config \t  "
        `shouldBe` cmd ViewConfig

    it "fails on additional input" $ do
      parseWith viewConfigParser "  config   some additional text 123  "
        `shouldBe` fail
    
    it "parses exact string" $ do
      parseWith viewConfigParser "config"
        `shouldBe` cmd ViewConfig

    it "case insensitive" $ do
      parseWith viewConfigParser "  ConFIg  "
        `shouldBe` cmd ViewConfig


  describe "updateConfigParser" $ do
    it "fails on empty string" $ do
      parseWith updateConfigParser ""
        `shouldBe` fail

    it "fails on absence of settings" $ do
      parseWith updateConfigParser "config"
        `shouldBe` fail

    it "fails on partial or redundant string" $ do
      parseWith updateConfigParser "conf color tc"
        `shouldBe` fail
      parseWith updateConfigParser "configg width 14"
        `shouldBe` fail
      parseWith updateConfigParser "config123 heigh 22"
        `shouldBe` fail

    it "fails on additional input" $ do
      parseWith updateConfigParser "  config  color tc abc "
        `shouldBe` fail

    it "ignores whitespace on both sides" $ do
      parseWith updateConfigParser "   config c tc w 14 h 7   "
        `shouldBe` cmd (color TrueColor <> width 14 <> height 7) 


  describe "quitParser" $ do
    it "fails on empty string" $ do
      parseWith quitParser ""
        `shouldBe` fail

    it "fails on partial or redundant string" $ do
      parseWith quitParser "qui"
        `shouldBe` fail
      parseWith quitParser "quitt"
        `shouldBe` fail
      parseWith quitParser "quit123"
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


-- | Basically this is an invalid semigroup, but for us it works as expected
-- with the provided combinators.
instance Semigroup Command where
  (UpdateConfig c1 w1 h1) <> (UpdateConfig c2 w2 h2) =
    UpdateConfig
      (maybe c2 Just c1)
      (maybe w2 Just w1)
      (maybe h2 Just h1)

color :: ColorScheme -> Command
color clr = UpdateConfig (Just clr) Nothing Nothing

width :: Int -> Command
width w = UpdateConfig Nothing (Just w) Nothing

height :: Int -> Command
height h = UpdateConfig Nothing Nothing (Just h)