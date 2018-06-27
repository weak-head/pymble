{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.PrettyPrint.TelnetSpec where

import Test.Hspec
import Test.QuickCheck

import Control.Exception (evaluate)
import Control.Monad (forM_)

import Pymble.PrettyPrint.Telnet
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ColorScheme" $ do
    it "is comparable" $ do
      (Color16 == TrueColor) 
        `shouldBe` False
      (Xterm256 == Grayscale) 
        `shouldBe` False
      (TrueColor == TrueColor)
        `shouldBe` True

    it "is convertible" $ do
      forM_ [Color16, Xterm256, Grayscale, TrueColor] $ \color -> do
        (read $ show color) `shouldBe` color

    it "could be parsed from custom string" $ do
      read "16"  
        `shouldBe` Color16
      read "256" 
        `shouldBe` Xterm256
      read "gs"  
        `shouldBe` Grayscale
      read "tc"  
        `shouldBe` TrueColor
      evaluate(read@ColorScheme "")    
        `shouldThrow` errorCall "Prelude.read: no parse"
      evaluate(read@ColorScheme "abd") 
        `shouldThrow` errorCall "Prelude.read: no parse"

  describe "encodeChar" $ do
    it "returns same char" $ do
      (encodeChar 'c' "") `shouldBe` "c"

  describe "encodeReset" $ do
    it "encodes terminal reset" $ do
      (encodeReset "") `shouldBe` "\ESC[0;00m"

  describe "termClear" $ do
    it "encodes terminal clear" $ do
      (termClear "") `shouldBe` "\ESC[2J"