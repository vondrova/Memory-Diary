{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Domain.Filter'
--
--   Tests cover the pure filtering logic applied to memory search results:
--
--   * parseUtcTime    — parses date/datetime strings in several formats;
--                         returns 'Nothing' for invalid or empty input
--   * matchesFilters — checks whether a 'MemoryRow' satisfies all active
--                         filters (free-text, tag, location, date range);
--                         search is case-insensitive and matches substrings
--   * validFilterRange — validates that a date range is not reversed
module Domain.FilterSpec (spec) where

import Domain.Filter
import Test.Hspec
import TestHelpers (atDay, atTimeSec)
import Types

spec :: Spec
spec = do
  describe "parseUtcTime" $ do
    it "parses datetime with hours and minutes" $
      parseUtcTime "2026-06-14T10:30"
        `shouldBe` Just (atTimeSec 2026 6 14 10 30 0)

    it "parses datetime with seconds" $
      parseUtcTime "2026-06-14T10:30:45"
        `shouldBe` Just (atTimeSec 2026 6 14 10 30 45)

    it "parses a date-only value (midnight)" $
      parseUtcTime "2026-06-14"
        `shouldBe` Just (atDay 2026 6 14)

    it "returns Nothing for an invalid value" $
      parseUtcTime "not-a-date" `shouldBe` Nothing

    it "returns Nothing for an empty string" $
      parseUtcTime "" `shouldBe` Nothing

  describe "matchesFilters" $ do
    it "accepts every memory when no filters are given" $
      matchesFilters Nothing Nothing Nothing Nothing Nothing basicMem `shouldBe` True

    it "matches on title substring (case-insensitive)" $ do
      matchesFilters (Just "trip") Nothing Nothing Nothing Nothing basicMem `shouldBe` True
      matchesFilters (Just "TRIP") Nothing Nothing Nothing Nothing basicMem `shouldBe` True
      matchesFilters (Just "nope") Nothing Nothing Nothing Nothing basicMem `shouldBe` False

    it "also matches on description substring" $
      matchesFilters (Just "nice") Nothing Nothing Nothing Nothing basicMem `shouldBe` True

    it "matches on tag substring (case-insensitive)" $ do
      matchesFilters Nothing (Just "trip") Nothing Nothing Nothing basicMem `shouldBe` True
      matchesFilters Nothing (Just "sum") Nothing Nothing Nothing basicMem `shouldBe` True
      matchesFilters Nothing (Just "missing") Nothing Nothing Nothing basicMem `shouldBe` False

    it "matches on location substring (case-insensitive)" $ do
      matchesFilters Nothing Nothing (Just "pra") Nothing Nothing basicMem `shouldBe` True
      matchesFilters Nothing Nothing (Just "PRAGUE") Nothing Nothing basicMem `shouldBe` True
      matchesFilters Nothing Nothing (Just "london") Nothing Nothing basicMem `shouldBe` False

    it "rejects a memory with no location when a location filter is given" $
      matchesFilters
        Nothing
        Nothing
        (Just "Prague")
        Nothing
        Nothing
        basicMem {mrLocation = Nothing}
        `shouldBe` False

    it "filters by lower date bound (inclusive)" $ do
      matchesFilters Nothing Nothing Nothing (Just "2026-01-01") Nothing basicMem `shouldBe` True
      matchesFilters Nothing Nothing Nothing (Just "2026-02-01") Nothing basicMem `shouldBe` False

    it "filters by upper date bound (inclusive)" $ do
      matchesFilters Nothing Nothing Nothing Nothing (Just "2026-01-02") basicMem `shouldBe` True
      matchesFilters Nothing Nothing Nothing Nothing (Just "2026-01-01") basicMem `shouldBe` False

    it "ignores an unparseable date bound" $ do
      matchesFilters Nothing Nothing Nothing (Just "bad-date") Nothing basicMem `shouldBe` True
      matchesFilters Nothing Nothing Nothing Nothing (Just "bad-date") basicMem `shouldBe` True

  describe "validFilterRange" $ do
    it "accepts an empty range" $
      validFilterRange Nothing Nothing `shouldBe` True

    it "accepts a forward date range" $
      validFilterRange (Just "2026-01-01") (Just "2026-01-31") `shouldBe` True

    it "rejects a reversed date range" $
      validFilterRange (Just "2026-02-01") (Just "2026-01-31") `shouldBe` False

    it "ignores unparseable bounds" $
      validFilterRange (Just "bad-date") (Just "2026-01-31") `shouldBe` True

  describe "applyMemoryFilters" $ do
    it "returns all rows when no filters are given" $
      length (applyMemoryFilters Nothing Nothing Nothing Nothing Nothing twoMems) `shouldBe` 2

    it "keeps only the matching row" $
      applyMemoryFilters (Just "trip") Nothing Nothing Nothing Nothing twoMems
        `shouldBe` [basicMem]

    it "returns empty list when nothing matches" $
      applyMemoryFilters (Just "xyz") Nothing Nothing Nothing Nothing twoMems
        `shouldBe` []

basicMem :: MemoryRow
basicMem =
  MemoryRow
    { mrId = 1,
      mrTitle = "First trip",
      mrTimeFrom = atDay 2026 1 1,
      mrTimeTo = atDay 2026 1 2,
      mrDescription = Just "nice holiday",
      mrLocation = Just "Prague",
      mrTags = ["trip", "summer"],
      mrPhotos = []
    }

twoMems :: [MemoryRow]
twoMems =
  [ basicMem,
    basicMem {mrId = 2, mrTitle = "Holiday", mrTags = ["holiday"]}
  ]
