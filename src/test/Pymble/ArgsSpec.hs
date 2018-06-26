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
  
  describe "ArgsSpec" $ do
    it "works" $ do
      1 `shouldBe` 1

--- helpers ----------------------------

parseMode :: [String] -> StartupMode
parseMode = runParser startupMode

parseTelnet :: [String] -> StartupMode
parseTelnet = runParser telnetServer

parseDirect :: [String] -> StartupMode
parseDirect = runParser directConvert

runParser :: P.Parser a -> [String] -> a
runParser parser arg = 
  case P.execParserPure P.defaultPrefs (P.info parser P.fullDesc) arg of
    P.Success a           -> a
    P.CompletionInvoked _ -> error "completion requested"
    P.Failure _           -> error "failed to parse"