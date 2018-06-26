{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pymble.ArgsSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Semigroup
import Data.Maybe (maybe)
import qualified Options.Applicative as P 

import Pymble.Args
import Pymble.PrettyPrint.Telnet (ColorScheme(..))
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  describe "telnetServer" $ do
    it "parses short port" $ do
      parseTelnet ["-p", "88"]
        `shouldBe` Right (TelnetServer 88)
    
    it "parses long port" $ do
      parseTelnet ["--port", "99"]
        `shouldBe` Right (TelnetServer 99)

    it "defaults to 23 port" $ do
      parseTelnet []
        `shouldBe` Right (TelnetServer 23)

    it "prohibits two port switches" $ do
      parseTelnet ["-p", "77", "-p", "44"]
        `shouldBe` parseFailure

    it "fails on direct convert string" $ do
      parseTelnet ["-c", "gs", "-w", "40", "img.png"]
        `shouldBe` parseFailure


  describe "directConvert" $ do
    it "fails on empty input" $ do
      parseDirect []
        `shouldBe` parseFailure

    it "accepts single url" $ do
      parseDirect ["img.png"]
        `shouldBe` Right (url "img.png")

    it "handles short color and url" $ do
      parseDirect ["-c", "16", "img.png"] 
        `shouldBe` Right (url "img.png" <> color Color16)



--- helpers ----------------------------

parseMode :: [String] -> Either String StartupMode
parseMode = runParser startupMode

parseTelnet :: [String] -> Either String StartupMode
parseTelnet = runParser telnetServer

parseDirect :: [String] -> Either String StartupMode
parseDirect = runParser directConvert

runParser :: P.Parser a -> [String] -> Either String a
runParser parser arg = 
  case P.execParserPure P.defaultPrefs (P.info parser P.fullDesc) arg of
    P.Success a           -> Right a
    P.CompletionInvoked _ -> completionRequested
    P.Failure _           -> parseFailure

parseFailure        = Left "failed to parse"
completionRequested = Left "completion requested"

-- | Basically this is an invalid semigroup, but for us it works as expected
-- with the provided combinators.
instance Semigroup StartupMode where
  (DirectConvert w1 h1 c1 u1) <> (DirectConvert w2 h2 c2 u2) =
    DirectConvert
      (maybe w2 (Just) w1)
      (maybe h2 (Just) h1)
      (maybe c2 (Just) c1)
      (if null u1 then u2 else u1)

url :: String -> StartupMode
url = DirectConvert Nothing Nothing Nothing

color :: ColorScheme -> StartupMode
color clr = DirectConvert Nothing Nothing (Just clr) []

width :: Int -> StartupMode
width w = DirectConvert (Just w) Nothing Nothing []

height :: Int -> StartupMode
height h = DirectConvert Nothing (Just h) Nothing []
