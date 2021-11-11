module TransunionSpec (spec) where

import qualified Data.Map as Map
import Transunion
import Parser
import Util

import Control.Exception (evaluate)
import Test.Hspec

spec :: Spec
spec = do
  parserSpec
  stringSpec

parserSpec :: Spec
parserSpec = do
  describe "signalParser" $ do
    it "parses out a Signal and it's content" $ do
      let toParse = "AD02 " ++ replicate 123 'a'
          result = parseSignal toParse
      (fst . head) result `shouldBe` "AD02"

    it "parses out Signals into pieces" $ do
      let toParse = "AD02" ++ replicate 123 'a' ++
                    "AH11" ++ replicate 73 'a' ++
                    "VS01" ++ replicate 29 'a'

          result = map fst $ parseSignal toParse
      result `shouldBe` ["AD02", "AH11", "VS01"]

  describe "reconciling" $ do
    it "can zip together displacements" $
      cutSegments [1,2,3] "abbccc" `shouldBe` ["a", "bb", "ccc"]

    it "can reconcile a single segment" $ do
      let parsed_string = "062311                        1201F 0273814620150824124331"
          expected = ("TU4R", [
                ("segment_length", "062")
              , ("version_switch", "3")
              , ("country_code", "1")
              , ("language_indicator", "1")
              , ("user_reference_number", "                        ")
              , ("bureau_market", "12")
              , ("bureau_submarket", "01")
              , ("industry_code", "F ")
              , ("inquiring_subscriber_code", "02738146")
              , ("transaction_date", "20150824")
              , ("transaction_time", "124331")
              ])
      reconcileSegment ("TU4R", parsed_string) `shouldBe` expected

    it "parses out the segments" $ do
      let inputString = "TU4R062311                        1201F 0273814620150824124331"
          expected = [("TU4R", [
                ("segment_length", "062")
              , ("version_switch", "3")
              , ("country_code", "1")
              , ("language_indicator", "1")
              , ("user_reference_number", "                        ")
              , ("bureau_market", "12")
              , ("bureau_submarket", "01")
              , ("industry_code", "F ")
              , ("inquiring_subscriber_code", "02738146")
              , ("transaction_date", "20150824")
              , ("transaction_time", "124331")
              ])]
      parse inputString `shouldBe` expected

    it "shows where an error happened" $ do
      let inputString = "bad ffr"

      evaluate(parse inputString) `shouldThrow` anyErrorCall


stringSpec :: Spec
stringSpec =
  describe "#snakeCase | #titleCase" $ do
    it "can snakecase" $ do
      let thing = map titleToSnakeCase ["Some Title", "And Stuff"]
          expected = ["some_title", "and_stuff"]
      thing `shouldBe` expected

    it "can titleize" $ do
      let thing = snakeToTitleCase "some_title"
          expected = "Some Title"
      thing `shouldBe` expected
