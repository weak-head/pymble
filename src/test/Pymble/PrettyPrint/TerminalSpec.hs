{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.PrettyPrint.TerminalSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Exception (evaluate)
import Control.Monad (forM_)

import           Pymble.PrettyPrint.Terminal
import qualified Pymble.PrettyPrint.Terminal.Color as TC
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ColorScheme" $ do
    it "is comparable" $ do
      (Color16 == TrueColor)   `shouldBe` False
      (Xterm256 == Grayscale)  `shouldBe` False
      (TrueColor == TrueColor) `shouldBe` True

    it "is convertible" $ do
      forM_ [Color16, Xterm256, Grayscale, TrueColor] $ \color -> do
        (read $ show color) `shouldBe` color

    it "could be parsed from custom string" $ do
      read "16"  `shouldBe` Color16
      read "256" `shouldBe` Xterm256
      read "gs"  `shouldBe` Grayscale
      read "tc"  `shouldBe` TrueColor
      evaluate(read @ColorScheme "")    
        `shouldThrow` errorCall "Prelude.read: no parse"
      evaluate(read @ColorScheme "abd") 
        `shouldThrow` errorCall "Prelude.read: no parse"

  describe "encodeChar" $ do
    it "returns same char" $ do
      (encodeChar 'c' "") `shouldBe` "c"

  describe "encodeReset" $ do
    it "encodes reset" $ do
      (encodeReset "") `shouldBe` "\ESC[0;00m"

  describe "termClear" $ do
    it "encodes clear" $ do
      (termClear "") `shouldBe` "\ESC[2J"

  describe "encodeTerminalColor" $ do
    it "encodes correct color" $ do
      let scenarios = [ ((TC.Color16 1),          "\ESC[31m")
                      , ((TC.Color16 10),         "\ESC[92m")
                      , ((TC.Xterm256 114),       "\ESC[38;5;114m")
                      , ((TC.Grayscale 251),      "\ESC[38;5;251m")
                      , ((TC.TrueColor 10 20 30), "\ESC[38;2;10;20;30m") ]
      forM_ scenarios $ \(color, expectation) ->
        (encodeTerminalColor color "") `shouldBe` expectation

  describe "encodeColoredChar" $ do
    it "encodes with color and reset" $ do
      encodeColoredChar (TC.Color16 2) 't' ""
        `shouldBe` "\ESC[32mt\ESC[0;00m"

  describe "encodeColoredString" $ do
    it "encodes with color and reset" $ do
      encodeColoredString (TC.Xterm256 90) "the string" ""
        `shouldBe` "\ESC[38;5;90mthe string\ESC[0;00m"
