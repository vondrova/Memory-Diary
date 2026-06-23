{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'Domain.Validation'
--
--   Tests cover the pure validation rules shared by all handlers:
--
--   * Required text — rejects blank/whitespace-only values; trims accepted ones
--   * Optional text — normalises 'Nothing' and empty strings to 'Nothing'
--   * Memory validation — requires title; rejects reversed date intervals;
--                         validates the full 'MemoryInput' record
--   * Important-day validation — requires title
--   * Note validation — requires owner, title, and body; rejects display names
--                       as owner values (only "left", "right", "shared" allowed)
--   * Plan validation — requires title; accepts blank category (backend fills it in)
module Domain.ValidationSpec (spec) where

import Domain.Validation
import Test.Hspec
import TestHelpers (atDay)
import Types

spec :: Spec
spec = do
  describe "required text" $ do
    it "trims accepted values" $
      requireNonBlank "title" "  First trip  " `shouldBe` Right "First trip"

    it "rejects blank values" $
      requireNonBlank "title" "   " `shouldBe` Left (RequiredField "title")

    it "normalizes optional text fields" $ do
      cleanMaybeText (Just "  Prague  ") `shouldBe` Just "Prague"
      cleanMaybeText (Just "   ") `shouldBe` Nothing
      cleanMaybeText Nothing `shouldBe` Nothing

  describe "memory validation" $ do
    it "accepts a valid memory interval" $
      validateMemoryRange (atDay 2026 1 1) (atDay 2026 1 2) `shouldBe` Right ()

    it "rejects a reversed memory interval" $
      validateMemoryRange (atDay 2026 1 2) (atDay 2026 1 1) `shouldBe` Left InvalidTimeRange

    it "validates the whole memory input" $
      validateMemoryInputFields validMemory `shouldBe` Right ()

    it "requires a memory title" $
      validateMemoryInputFields validMemory {miTitle = " "} `shouldBe` Left (RequiredField "title")

    it "rejects reversed memory dates in the whole input" $
      validateMemoryInputFields validMemory {miTimeFrom = atDay 2026 1 3, miTimeTo = atDay 2026 1 2}
        `shouldBe` Left InvalidTimeRange

  describe "important day validation" $
    it "requires a title" $
      validateImportantDayInputFields (ImportantDayInput "" (atDay 2026 6 18) Nothing)
        `shouldBe` Left (RequiredField "title")

  describe "note validation" $ do
    it "accepts a complete note" $
      validateNoteInputFields (NoteInput "shared" "Allergy" "Peanuts") `shouldBe` Right ()

    it "requires note owner, title and body" $ do
      validateNoteInputFields (NoteInput "" "Allergy" "Peanuts") `shouldBe` Left InvalidNoteOwner
      validateNoteInputFields (NoteInput "shared" "" "Peanuts") `shouldBe` Left (RequiredField "title")
      validateNoteInputFields (NoteInput "shared" "Allergy" "") `shouldBe` Left (RequiredField "body")

    it "rejects display names as note owners" $
      validateNoteInputFields (NoteInput "A" "Allergy" "Peanuts") `shouldBe` Left InvalidNoteOwner

  describe "plan validation" $ do
    it "accepts a complete plan" $
      validatePlanInputFields (PlanInput "Trips" "Visit Brno" Nothing False) `shouldBe` Right ()

    it "accepts a plan with blank category (backend normalises it to Other)" $
      validatePlanInputFields (PlanInput "" "Visit Brno" Nothing False) `shouldBe` Right ()

    it "requires a title" $
      validatePlanInputFields (PlanInput "Trips" "" Nothing False) `shouldBe` Left (RequiredField "title")

  describe "diary validation" $ do
    it "accepts entries owned by either profile" $ do
      validateDiaryInputFields (DiaryInput "left" (atDay 2026 1 1) "Entry") `shouldBe` Right ()
      validateDiaryInputFields (DiaryInput " RIGHT " (atDay 2026 1 1) "Entry") `shouldBe` Right ()

    it "rejects unknown owners and blank content" $ do
      validateDiaryInputFields (DiaryInput "shared" (atDay 2026 1 1) "Entry") `shouldBe` Left InvalidDiaryOwner
      validateDiaryInputFields (DiaryInput "left" (atDay 2026 1 1) "   ") `shouldBe` Left (RequiredField "body")

  describe "rename validation" $ do
    it "accepts a non-blank new name" $
      validateRenameInputFields (RenameInput "new tag") `shouldBe` Right ()

    it "rejects a blank new name" $
      validateRenameInputFields (RenameInput "") `shouldBe` Left (RequiredField "name")

    it "rejects a whitespace-only new name" $
      validateRenameInputFields (RenameInput "   ") `shouldBe` Left (RequiredField "name")

  describe "error rendering" $
    it "turns validation errors into user-facing API messages" $ do
      renderValidationError (RequiredField "title") `shouldBe` "Missing required field: title"
      renderValidationError InvalidTimeRange `shouldBe` "Memory end time must not be before its start time."
      renderValidationError InvalidDiaryOwner `shouldBe` "Diary owner must be left or right."

validMemory :: MemoryInput
validMemory =
  MemoryInput
    { miTitle = "First trip",
      miTimeFrom = atDay 2026 1 1,
      miTimeTo = atDay 2026 1 2,
      miDescription = Just "Nice",
      miLocation = Just "Prague",
      miTags = ["trip"],
      miPhotos = []
    }
