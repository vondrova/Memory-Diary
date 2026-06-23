module LibSpec (spec) where

import Data.Text (pack)
import Data.Time.Calendar (diffDays, fromGregorian)
import Test.Hspec
import Lib
  ( computeNextOccurrence
  , computeStats
  , durMins
  , joinComma
  , monthDay
  , parseDay
  , splitComma
  , topN
  )

spec :: Spec
spec = do
  describe "durMins" $ do
    it "returns 0 for identical timestamps" $
      durMins "2024-06-01T10:00" "2024-06-01T10:00" `shouldBe` 0

    it "computes 60 minutes for a one-hour span" $
      durMins "2024-06-01T10:00" "2024-06-01T11:00" `shouldBe` 60

    it "computes 1440 minutes for a full day" $
      durMins "2024-06-01T00:00" "2024-06-02T00:00" `shouldBe` 1440

    it "handles an overnight span correctly" $
      durMins "2024-06-01T23:00" "2024-06-02T01:00" `shouldBe` 120

    it "returns 0 when end is before start" $
      durMins "2024-06-01T12:00" "2024-06-01T10:00" `shouldBe` 0

    it "returns 0 for unparseable timestamps" $
      durMins "not-a-date" "also-not" `shouldBe` 0

  describe "topN" $ do
    it "returns most frequent items first" $
      topN 2 ["a", "b", "a", "c", "a", "b"] `shouldBe` [("a", 3), ("b", 2)]

    it "limits results to n" $
      length (topN 2 ["a", "b", "c"]) `shouldBe` 2

    it "returns empty list for empty input" $
      topN 5 [] `shouldBe` []

    it "handles n larger than the list" $
      length (topN 10 ["x", "y"]) `shouldBe` 2

    it "counts single-occurrence items" $
      topN 3 ["a"] `shouldBe` [("a", 1)]

  describe "splitComma / joinComma" $ do
    it "roundtrips a list of tags" $
      splitComma (joinComma ["elm", "haskell", "fp"]) `shouldBe` ["elm", "haskell", "fp"]

    it "returns empty list for empty text" $
      splitComma (pack "") `shouldBe` []

    it "handles a single item" $
      splitComma (joinComma ["solo"]) `shouldBe` ["solo"]

    it "roundtrips a list with spaces in tags" $
      splitComma (joinComma ["new york", "road trip"]) `shouldBe` ["new york", "road trip"]

  describe "computeNextOccurrence" $ do
    it "returns this year when anniversary is still upcoming" $ do
      let today              = fromGregorian 2024 6 1
      let (next, days, m, d) = computeNextOccurrence today "2020-12-25"
      next `shouldBe` "2024-12-25"
      days `shouldSatisfy` (> 0)
      m    `shouldBe` 12
      d    `shouldBe` 25

    it "returns next year when anniversary has already passed" $ do
      let today              = fromGregorian 2024 6 15
      let (next, days, m, d) = computeNextOccurrence today "2020-06-01"
      next `shouldBe` "2025-06-01"
      days `shouldSatisfy` (> 0)
      m    `shouldBe` 6
      d    `shouldBe` 1

    it "returns today with 0 days when anniversary is today" $ do
      let today              = fromGregorian 2024 6 15
      let (next, days, _, _) = computeNextOccurrence today "2020-06-15"
      next `shouldBe` "2024-06-15"
      days `shouldBe` 0

    it "handles a December anniversary when today is January" $ do
      let today              = fromGregorian 2024 1 5
      let (next, days, m, d) = computeNextOccurrence today "2019-12-31"
      next `shouldBe` "2024-12-31"
      days `shouldSatisfy` (> 0)
      m    `shouldBe` 12
      d    `shouldBe` 31

    it "returns fallback for unparseable date" $ do
      let today              = fromGregorian 2024 1 1
      let (next, days, m, d) = computeNextOccurrence today "not-a-date"
      next `shouldBe` "not-a-date"
      days `shouldBe` 0
      m    `shouldBe` 0
      d    `shouldBe` 0

  describe "parseDay" $ do
    it "parses a valid ISO date" $
      parseDay "2024-06-15" `shouldBe` Just (fromGregorian 2024 6 15)

    it "returns Nothing for an invalid date string" $
      parseDay "not-a-date" `shouldBe` Nothing

    it "returns Nothing for wrong date format" $
      parseDay "15-06-2024" `shouldBe` Nothing

    it "parses first day of year" $
      parseDay "2024-01-01" `shouldBe` Just (fromGregorian 2024 1 1)

  describe "days together (diffDays)" $ do
    it "computes 366 days between two dates one year apart (leap year)" $ do
      let start = fromGregorian 2023 6 15
      let end   = fromGregorian 2024 6 15
      diffDays end start `shouldBe` 366

    it "computes 365 days for a non-leap year" $ do
      let start = fromGregorian 2022 6 15
      let end   = fromGregorian 2023 6 15
      diffDays end start `shouldBe` 365

    it "computes 0 days for same date" $ do
      let d = fromGregorian 2024 1 1
      diffDays d d `shouldBe` 0

  describe "monthDay" $ do
    it "extracts MM-DD from a YYYY-MM-DD date string" $
      monthDay "2024-06-23" `shouldBe` "06-23"

    it "extracts MM-DD from a full datetime string" $
      monthDay "2024-06-23T10:30" `shouldBe` "06-23"

    it "matches two dates with the same month and day" $
      monthDay "2020-12-25" `shouldBe` monthDay "2024-12-25"

    it "does not match dates with different month or day" $
      monthDay "2024-06-23" `shouldSatisfy` (/= monthDay "2024-06-24")

    it "works for January 1st" $
      monthDay "2024-01-01" `shouldBe` "01-01"

  describe "computeStats" $ do
    it "returns zero stats for empty list" $ do
      let s = computeStats []
      statsTotalMinutes s   `shouldBe` 0
      statsMemoryCount s    `shouldBe` 0
      statsPhotoCount s     `shouldBe` 0
      statsVisitedPlaces s  `shouldBe` 0
      statsAverageMinutes s `shouldBe` 0

    it "counts memories correctly" $ do
      let mems = [ makeMemory 1 "A" "2024-06-01T10:00" "2024-06-01T11:00" Nothing [] []
                 , makeMemory 2 "B" "2024-06-02T10:00" "2024-06-02T12:00" Nothing [] []
                 ]
      statsMemoryCount (computeStats mems) `shouldBe` 2

    it "sums total minutes across memories" $ do
      let mems = [ makeMemory 1 "A" "2024-06-01T10:00" "2024-06-01T11:00" Nothing [] []
                 , makeMemory 2 "B" "2024-06-02T10:00" "2024-06-02T12:00" Nothing [] []
                 ]
      statsTotalMinutes (computeStats mems) `shouldBe` 180

    it "counts unique locations" $ do
      let mems = [ makeMemory 1 "A" "2024-06-01T10:00" "2024-06-01T11:00" (Just "Praha") [] []
                 , makeMemory 2 "B" "2024-06-02T10:00" "2024-06-02T11:00" (Just "Brno") [] []
                 , makeMemory 3 "C" "2024-06-03T10:00" "2024-06-03T11:00" (Just "Praha") [] []
                 ]
      statsVisitedPlaces (computeStats mems) `shouldBe` 2

    it "counts total photos" $ do
      let mems = [ makeMemory 1 "A" "2024-06-01T10:00" "2024-06-01T11:00" Nothing [] ["p1.jpg", "p2.jpg"]
                 , makeMemory 2 "B" "2024-06-02T10:00" "2024-06-02T11:00" Nothing [] ["p3.jpg"]
                 ]
      statsPhotoCount (computeStats mems) `shouldBe` 3


makeMemory :: Int -> String -> String -> String -> Maybe String -> [String] -> [String] -> Memory
makeMemory i t from to loc tags photos = Memory
  { memoryId          = i
  , memoryTitle       = t
  , memoryTimeFrom    = from
  , memoryTimeTo      = to
  , memoryDescription = Nothing
  , memoryLocation    = loc
  , memoryTags        = tags
  , memoryPhotos      = photos
  }
