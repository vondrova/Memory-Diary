{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Handler.Helpers'
--
--   Tests cover HTTP-layer utility functions that are too small to warrant
--   a database but are used across multiple handler modules:
--
--   * dedupeText   — removes duplicate 'Text' values while preserving the
--                      relative order of the first occurrence of each value
--   * extractName  — extracts a human-readable label from a raw JSON trash
--                      payload (looks for "title" then "name"; falls back
--                      to an em-dash for malformed payloads)
module Handler.HelpersSpec (spec) where

import Data.Text (Text)
import Handler.Helpers (dedupeText, extractName)
import Test.Hspec

spec :: Spec
spec = do
  describe "dedupeText" $ do
    it "keeps a list with no duplicates unchanged" $
      dedupeText ["a", "b", "c"] `shouldBe` (["a", "b", "c"] :: [Text])

    it "removes duplicate values, keeping relative order of survivors" $
      dedupeText ["photo1.jpg", "photo2.jpg", "photo1.jpg"]
        `shouldBe` (["photo1.jpg", "photo2.jpg"] :: [Text])

    it "handles an empty list" $
      dedupeText ([] :: [Text]) `shouldBe` []

    it "reduces a list of all-same values to a singleton" $
      dedupeText ["tag", "tag", "tag"] `shouldBe` (["tag"] :: [Text])

  describe "extractName" $ do
    it "extracts the title field from a memory payload" $
      extractName "{\"title\":\"Trip to Rome\"}" `shouldBe` "Trip to Rome"

    it "extracts the name field from a legacy important-day payload" $
      extractName "{\"name\":\"Birthday\"}" `shouldBe` "Birthday"

    it "prefers title over name when both are present" $
      extractName "{\"title\":\"T\",\"name\":\"N\"}" `shouldBe` "T"

    it "falls back to an em-dash for payloads with neither field" $
      extractName "{\"body\":\"no name here\"}" `shouldBe` "\8212"

    it "falls back to an em-dash for malformed JSON" $
      extractName "not json at all" `shouldBe` "\8212"
