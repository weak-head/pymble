{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pymble.PrettyPrint.Telnet.ColorSpec where

import Data.Word
import Test.Hspec
import Test.QuickCheck

import Pymble.PrettyPrint.Telnet.Color 
----------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "euclideanDistance" $ do
    it "for identical colors is zero" $ do
      let color = (100, 200, 250, 0)
      euclideanDistance color color `shouldBe` 0

    it "ignores alpha" $ do
      let c1 = (100, 200, 100, 50)
          c2 = (100, 200, 100, 25)
      euclideanDistance c1 c2 `shouldBe` 0

    it "measures rounded distance" $ do
      let c1 = (10, 10, 10, 0)
          c2 = (20, 20, 20, 0)
      euclideanDistance c1 c2 `shouldBe` 17
    
    it "rounds distance to nearest integral" $ do
      let c1 = (50, 50, 50, 0)
          c2 = (75, 75, 75, 0)
          c3 = (90, 75, 90, 0)
      euclideanDistance c1 c2 `shouldBe` 43  -- 43.30 -> 43.0
      euclideanDistance c1 c3 `shouldBe` 62  -- 61.84 -> 62.0

    it "can handle edge cases" $ do
      let c1 = (0,     0,   0, 0)
          c2 = (255, 255, 255, 0)
      euclideanDistance c1 c2 `shouldBe` 442