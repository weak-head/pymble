{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.Telnet.Api.ParserSpec where

import Control.Monad (forM_)
import Data.Semigroup
import Prelude hiding (fail)
import Test.Hspec
import Test.QuickCheck
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
        
    it "ignores whitespace in the middle" $ do
      parseWith updateConfigParser "  config    color     tc    width  \t\t 28"
        `shouldBe` cmd (color TrueColor <> width 28)

    it "parses exact string" $ do
      parseWith updateConfigParser "config color gs width 40 height 20"
        `shouldBe` cmd (color Grayscale <> width 40 <> height 20)
      parseWith updateConfigParser "config c 16 w 80 h 37"
        `shouldBe` cmd (color Color16 <> width 80 <> height 37)

    it "order invariant" $ do
      parseWith updateConfigParser "config color 256 width 20 height 18"
        `shouldBe` cmd (color Xterm256 <> width 20 <> height 18)
      parseWith updateConfigParser "config color 256 height 18 width 20"
        `shouldBe` cmd (color Xterm256 <> width 20 <> height 18)
      parseWith updateConfigParser "config width 20 color 256 height 18"
        `shouldBe` cmd (color Xterm256 <> width 20 <> height 18)
      parseWith updateConfigParser "config height 18 width 20 color 256"
        `shouldBe` cmd (color Xterm256 <> width 20 <> height 18)
      parseWith updateConfigParser "config height 18 color 256 width 20"
        `shouldBe` cmd (color Xterm256 <> width 20 <> height 18)

    it "parses different kinds of color input" $ do
      let scenarios = [ (cmd $ color Color16, "config color 16") 
                      , (cmd $ color Color16, "config c 16")
                      , (cmd $ color Color16, "CONFIG COLOR 16")
                      , (cmd $ color Xterm256, "config color 256")
                      , (cmd $ color Xterm256, "config c 256")
                      , (cmd $ color Xterm256, "COnfIG CoLoR 256")
                      , (cmd $ color Grayscale, "config color gs")
                      , (cmd $ color Grayscale, "config color grayscale")
                      , (cmd $ color Grayscale, "config color GRAYSCALE")
                      , (cmd $ color TrueColor, "config color tc")
                      , (cmd $ color TrueColor, "config color TrueColor")
                      ] 
      forM_ scenarios $ \(expectation, input) ->
        parseWith updateConfigParser input `shouldBe` expectation

    it "parses singleton width" $ do
      parseWith updateConfigParser "config width 14"
        `shouldBe` cmd (width 14)
      parseWith updateConfigParser "config w 28"
        `shouldBe` cmd (width 28)

    it "parses singleton height" $ do
      parseWith updateConfigParser "config height 33"
        `shouldBe` cmd (height 33)
      parseWith updateConfigParser "config h 77"
        `shouldBe` cmd (height 77)


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
  (UpdateConfig (RenderSettings c1 w1 h1)) <> (UpdateConfig (RenderSettings c2 w2 h2)) =
    UpdateConfig $ RenderSettings
      (maybe c2 Just c1)
      (maybe w2 Just w1)
      (maybe h2 Just h1)

color :: ColorScheme -> Command
color clr = UpdateConfig $ RenderSettings (Just clr) Nothing Nothing

width :: Int -> Command
width w = UpdateConfig $ RenderSettings Nothing (Just w) Nothing

height :: Int -> Command
height h = UpdateConfig $ RenderSettings Nothing Nothing (Just h)