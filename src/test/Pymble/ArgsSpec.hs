{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pymble.ArgsSpec where

import Test.Hspec
import Test.QuickCheck

import           Data.Semigroup
import           Data.Maybe (maybe)
import           Data.Bool (bool)
import qualified Options.Applicative as P 

import Pymble.Args
import Pymble.PrettyPrint.Telnet (ColorScheme(..))
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "StartupMode" $ do
    it "is comparable" $ do
      (TelnetServer 23 == TelnetServer 24)
        `shouldBe` False
      (TelnetServer 14 == url "img.png")
        `shouldBe` False
      ((url "img" <> width 14) == (url "img"))
        `shouldBe` False
      (TelnetServer 44 == TelnetServer 44)
        `shouldBe` True
      ((url "img" <> height 44) == (height 44 <> url "img"))
        `shouldBe` True
    it "is showable" $ do
      show (url "img" <> width 14 <> height 7 <> color Color16)
        `shouldBe` "DirectConvert (Just 14) (Just 7) (Just 16) \"img\""
      show (TelnetServer 172)
        `shouldBe` "TelnetServer 172"


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

    it "parses single url" $ do
      parseDirect ["img.png"]
        `shouldBe` Right (url "img.png")

    it "parses short color and url" $ do
      parseDirect ["-c", "16", "img.png"] 
        `shouldBe` Right (url "img.png" <> color Color16)

    it "parses long color and url" $ do
      parseDirect ["--color", "16", "img.png"]
        `shouldBe` Right (url "img.png" <> color Color16)

    it "parses all color schemes" $ do
      parseDirect ["-c", "16", "img.png"]
        `shouldBe` Right (url "img.png" <> color Color16)
      parseDirect ["-c", "256", "img.png"]
        `shouldBe` Right (url "img.png" <> color Xterm256)
      parseDirect ["-c", "gs", "img.png"]
        `shouldBe` Right (url "img.png" <> color Grayscale)
      parseDirect ["-c", "tc", "img.png"]
        `shouldBe` Right (url "img.png" <> color TrueColor)

    it "parses short width" $ do
      parseDirect ["-w", "77", "img.png"]
        `shouldBe` Right (url "img.png" <> width 77)

    it "parses long widht" $ do
      parseDirect ["--width", "88", "img.png"]
        `shouldBe` Right (url "img.png" <> width 88)

    it "parses short height" $ do
      parseDirect ["-h", "14", "img.png"]
        `shouldBe` Right (url "img.png" <> height 14)

    it "parses long height" $ do
      parseDirect ["--height", "45", "img.png"]
        `shouldBe` Right (url "img.png" <> height 45)

    it "parses full short string" $ do
      parseDirect ["-c", "256", "-w", "45", "-h", "25", "img.png"]
        `shouldBe` Right (  url "img.png"
                         <> color Xterm256
                         <> width 45
                         <> height 25)

    it "parses full long string" $ do
      parseDirect ["--width", "155", "--color", "tc", "--height", "77", "img.png"]
        `shouldBe` Right (  url "img.png"
                         <> color TrueColor
                         <> width 155
                         <> height 77)

    it "fails on input with double url" $ do
      parseDirect ["-c", "256", "-w", "45", "-h", "25", "img.png", "img2.png"]
        `shouldBe` parseFailure

    it "fails on input with unknown argument" $ do
      parseDirect ["-w", "77", "-b", "24", "img.png"]
        `shouldBe` parseFailure

  
  describe "startupMode" $ do
    it "parses to telnet server" $ do
      parseStartupMode []
        `shouldBe` Right (TelnetServer 23)
      parseStartupMode ["-p", "77"]
        `shouldBe` Right (TelnetServer 77)
      parseStartupMode ["--port", "127"]
        `shouldBe` Right (TelnetServer 127)

    it "parses to direct convert" $ do
      parseStartupMode ["img.png"]
        `shouldBe` Right (url "img.png")
      parseStartupMode ["-w", "14", "img.png"]
        `shouldBe` Right (url "img.png" <> width 14) 
      parseStartupMode ["-w", "80", "-h", "45", "img.png"]
        `shouldBe` Right (url "img.png" <> width 80 <> height 45)
      parseStartupMode ["-c", "gs", "img.png"]
        `shouldBe` Right (url "img.png" <> color Grayscale)


--- helpers ----------------------------

parseStartupMode :: [String] -> Either String StartupMode
parseStartupMode = runParser startupMode

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
      (maybe w2 Just w1)
      (maybe h2 Just h1)
      (maybe c2 Just c1)
      (bool u1 u2 $ null u1)

url :: String -> StartupMode
url = DirectConvert Nothing Nothing Nothing

color :: ColorScheme -> StartupMode
color clr = DirectConvert Nothing Nothing (Just clr) []

width :: Int -> StartupMode
width w = DirectConvert (Just w) Nothing Nothing []

height :: Int -> StartupMode
height h = DirectConvert Nothing (Just h) Nothing []