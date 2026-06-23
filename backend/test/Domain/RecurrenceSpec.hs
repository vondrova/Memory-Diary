{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Domain.Recurrence'
--
--   Tests cover the pure date arithmetic used for recurring events
--   (anniversaries, monthiversaries, birthdays):
--
--   * Annual occurrences — next occurrence of a yearly event, ordinal counter,
--                          and correct rollover after the date has already
--                          passed this year
--   * Monthiversaries    — next monthly occurrence, ordinal counter, and
--                          correct clipping to shorter months
module Domain.RecurrenceSpec (spec) where

import Data.Time (fromGregorian)
import Domain.Recurrence
import Test.Hspec
import TestHelpers (atDay)

spec :: Spec
spec = do
  describe "annual occurrences" $ do
    it "returns this year's anniversary when it is still ahead" $
      nextAnnualOccurrence (atDay 2026 6 1) (atDay 2024 6 18)
        `shouldBe` atDay 2026 6 18

    it "returns next year's anniversary after this year's date passed" $
      nextAnnualOccurrence (atDay 2026 6 20) (atDay 2024 6 18)
        `shouldBe` atDay 2027 6 18

    it "counts ordinal anniversaries from the original start year" $
      ordinalYears (atDay 2024 6 18) (atDay 2027 6 18) `shouldBe` 3

  describe "monthiversaries" $ do
    it "returns this month's occurrence when it has not passed yet" $
      nextMonthiversary (atDay 2026 6 10) (atDay 2026 2 18)
        `shouldBe` atDay 2026 6 18

    it "returns next month's occurrence after this month's date passed" $
      nextMonthiversary (atDay 2026 6 20) (atDay 2026 2 18)
        `shouldBe` atDay 2026 7 18

    it "clips long-month starts to shorter months" $
      nextMonthiversary (atDay 2026 2 1) (atDay 2026 1 31)
        `shouldBe` atDay 2026 2 28

    it "counts ordinal monthiversaries" $ do
      diffMonths (fromGregorian 2026 2 18) (fromGregorian 2026 6 18) `shouldBe` 4
      ordinalMonths (atDay 2026 2 18) (atDay 2026 6 18) `shouldBe` 4
