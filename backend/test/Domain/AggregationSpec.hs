{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Domain.Aggregation'
--
--   Tests cover the pure statistics functions that power the api stats endpoint:
--
--   * memDurationMinutes — computes duration between two 'UTCTime' values
--   * countItems         — builds a frequency map over a list of values
--   * topByCount         — returns the top 10 most frequent items
--   * computeStats       — full aggregation over a list of 'MemoryRow's
--                            (total duration, tag counts, location counts,
--                            visited-place count, monthly activity breakdown)

module Domain.AggregationSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Domain.Aggregation
import Models (Stats (..))
import Test.Hspec
import TestHelpers (atDay, atHour)
import Types

spec :: Spec
spec = do
  describe "memDurationMinutes" $ do
    it "returns 0 for a memory that starts and ends at the same time" $
      memDurationMinutes (blankMem (atDay 2026 1 1) (atDay 2026 1 1)) `shouldBe` 0

    it "returns 1440 for a 24-hour memory" $
      memDurationMinutes (blankMem (atDay 2026 1 1) (atDay 2026 1 2)) `shouldBe` 1440

    it "returns 60 for a one-hour memory" $
      memDurationMinutes (blankMem (atDay 2026 1 1) (atHour 2026 1 1 1)) `shouldBe` 60

  describe "countItems" $ do
    it "returns empty list for empty input" $
      countItems ([] :: [Text]) `shouldBe` []

    it "counts a single repeated item" $
      lookup ("a" :: Text) (countItems ["a", "a", "a"]) `shouldBe` Just 3

    it "counts distinct items independently" $ do
      let result = countItems (["x", "y", "x"] :: [Text])
      lookup "x" result `shouldBe` Just 2
      lookup "y" result `shouldBe` Just 1

  describe "topByCount" $ do
    it "returns at most 10 items" $
      length (topByCount (zip (map show [1 :: Int .. 50]) [1 .. 50 :: Int])) `shouldBe` 10

    it "sorts by count descending" $ do
      let result = topByCount ([("low", 1), ("high", 5), ("mid", 3)] :: [(Text, Int)])
      map snd result `shouldBe` [5, 3, 1]

    it "returns all items when there are fewer than 10" $
      length (topByCount ([("a", 1), ("b", 2)] :: [(Text, Int)])) `shouldBe` 2

  describe "computeStats" $ do
    it "returns zero stats for an empty list" $ do
      let stats = computeStats []
      statsTotalMinutes stats `shouldBe` 0
      statsTopTags stats `shouldBe` []
      statsTopLocations stats `shouldBe` []
      statsMonthlyActivity stats `shouldBe` []

    it "sums durations across memories" $ do
      let stats =
            computeStats
              [ blankMem (atDay 2026 1 1) (atDay 2026 1 2),
                blankMem (atDay 2026 2 1) (atDay 2026 2 2)
              ]
      statsTotalMinutes stats `shouldBe` 2880

    it "counts tags across all memories" $ do
      let stats =
            computeStats
              [ (blankMem (atDay 2026 1 1) (atDay 2026 1 2)) {mrTags = ["trip", "summer"]},
                (blankMem (atDay 2026 2 1) (atDay 2026 2 2)) {mrTags = ["trip"]}
              ]
      lookup "trip" (statsTopTags stats) `shouldBe` Just 2
      lookup "summer" (statsTopTags stats) `shouldBe` Just 1

    it "counts locations across all memories" $ do
      let stats =
            computeStats
              [ (blankMem (atDay 2026 1 1) (atDay 2026 1 2)) {mrLocation = Just "Prague"},
                (blankMem (atDay 2026 2 1) (atDay 2026 2 2)) {mrLocation = Just "Prague"},
                (blankMem (atDay 2026 3 1) (atDay 2026 3 2)) {mrLocation = Just "Brno"}
              ]
      lookup "Prague" (statsTopLocations stats) `shouldBe` Just 2
      lookup "Brno" (statsTopLocations stats) `shouldBe` Just 1

    it "counts all unique visited places, not only the displayed top 10" $ do
      let rows =
            map
              (\i -> (blankMem (atDay 2026 1 i) (atDay 2026 1 i)) {mrLocation = Just ("Place " <> T.pack (show i))})
              [1 .. 12]
          stats = computeStats rows
      statsVisitedPlaces stats `shouldBe` 12
      length (statsTopLocations stats) `shouldBe` 10

    it "groups monthly activity by year-month" $ do
      let stats =
            computeStats
              [ blankMem (atDay 2026 1 1) (atDay 2026 1 2),
                blankMem (atDay 2026 1 5) (atDay 2026 1 6),
                blankMem (atDay 2026 3 1) (atDay 2026 3 2)
              ]
      lookup "2026-01" (statsMonthlyActivity stats) `shouldBe` Just 2
      lookup "2026-03" (statsMonthlyActivity stats) `shouldBe` Just 1

blankMem :: UTCTime -> UTCTime -> MemoryRow
blankMem from to =
  MemoryRow
    { mrId = 1,
      mrTitle = "Test memory",
      mrTimeFrom = from,
      mrTimeTo = to,
      mrDescription = Nothing,
      mrLocation = Nothing,
      mrTags = [],
      mrPhotos = []
    }
