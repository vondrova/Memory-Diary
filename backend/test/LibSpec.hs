module LibSpec (spec) where

import Data.Text (pack)
import Test.Hspec
import Lib (durMins, joinComma, splitComma, topN)

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
