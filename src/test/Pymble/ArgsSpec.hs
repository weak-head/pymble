{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.ArgsSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Options.Applicative as P 

import Pymble.Args
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  describe "telnetServer" $ do
    it "parses short port" $ do
      parseTelnet ["-p", "88"] `shouldBe` Right (TelnetServer 88)
    
    it "parses long port" $ do
      parseTelnet ["--port", "99"] `shouldBe` Right (TelnetServer 99)

    it "defaults to 23 port" $ do
      parseTelnet [] `shouldBe` Right (TelnetServer 23)

    it "prohibits two port switches" $ do
      parseTelnet ["-p", "77", "-p", "44"] `shouldBe` (Left "failed to parse")

    it "fails on direct convert string" $ do
      parseTelnet ["-c", "gs", "-w", "40", "img.png"] `shouldBe` (Left "failed to parse")


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
    P.CompletionInvoked _ -> Left "completion requested"
    P.Failure _           -> Left "failed to parse"