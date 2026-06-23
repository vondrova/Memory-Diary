module LibSpec (spec) where

import Data.Text (pack)
import Data.Time.Calendar (fromGregorian)
import Test.Hspec
import Lib (computeNextOccurrence, durMins, joinComma, splitComma, topN)

spec :: Spec
spec = do
  describe "durMins" $ do
    it "returns 0 for identical timestamps" $
      durMins "2024-06-01T10:00" "2024-06-01T10:00" `shouldBe` 0

    it "computes 60 minutes for a one-hour span" $
      durMins "2024-06-01T10:00" "2024-06-01T11:00" `shouldBe` 60

    it "computes 1440 minutes for a full day" $
      durMins "2024-06-01T00:00" "2024-06-02T00:00" `shouldBe` 1440

    it "returns 0 when end is before start" $
      durMins "2024-06-01T12:00" "2024-06-01T10:00" `shouldBe` 0

  describe "topN" $ do
    it "returns most frequent items first" $
      topN 2 ["a", "b", "a", "c", "a", "b"] `shouldBe` [("a", 3), ("b", 2)]

    it "limits results to n" $
      length (topN 2 ["a", "b", "c"]) `shouldBe` 2

    it "returns empty list for empty input" $
      topN 5 [] `shouldBe` []

  describe "splitComma / joinComma" $ do
    it "roundtrips a list of tags" $
      splitComma (joinComma ["elm", "haskell", "fp"]) `shouldBe` ["elm", "haskell", "fp"]

    it "returns empty list for empty text" $
      splitComma (pack "") `shouldBe` []

    it "handles a single item" $
      splitComma (joinComma ["solo"]) `shouldBe` ["solo"]

  describe "computeNextOccurrence" $ do
    it "returns this year when anniversary is still upcoming" $ do
      let today        = fromGregorian 2024 6 1
      let (next, days, m, d) = computeNextOccurrence today "2020-12-25"
      next `shouldBe` "2024-12-25"
      days `shouldSatisfy` (> 0)
      m    `shouldBe` 12
      d    `shouldBe` 25

    it "returns next year when anniversary has already passed" $ do
      let today        = fromGregorian 2024 6 15
      let (next, days, m, d) = computeNextOccurrence today "2020-06-01"
      next `shouldBe` "2025-06-01"
      days `shouldSatisfy` (> 0)
      m    `shouldBe` 6
      d    `shouldBe` 1

    it "returns today with 0 days when anniversary is today" $ do
      let today        = fromGregorian 2024 6 15
      let (next, days, _, _) = computeNextOccurrence today "2020-06-15"
      next `shouldBe` "2024-06-15"
      days `shouldBe` 0

    it "returns fallback for unparseable date" $ do
      let today = fromGregorian 2024 1 1
      let (next, days, m, d) = computeNextOccurrence today "not-a-date"
      next `shouldBe` "not-a-date"
      days `shouldBe` 0
      m    `shouldBe` 0
      d    `shouldBe` 0
