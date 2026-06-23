{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure validation rules shared by handlers and tests
module Domain.Validation
  ( ValidationError (..),
    renderValidationError,
    validateMemoryInputFields,
    validateImportantDayInputFields,
    validateNoteInputFields,
    validatePlanInputFields,
    validateDiaryInputFields,
    validateRenameInputFields,
    validateMemoryRange,
    requireNonBlank,
    cleanMaybeText,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Types

-- | Required field was blank, or the time range was reversed
data ValidationError
  = RequiredField Text
  | InvalidTimeRange
  | InvalidNoteOwner
  | InvalidDiaryOwner
  deriving stock (Eq, Show)

-- | Convert a 'ValidationError' into a user-facing error message
renderValidationError :: ValidationError -> Text
renderValidationError err =
  case err of
    RequiredField field ->
      "Missing required field: " <> field
    InvalidTimeRange ->
      "Memory end time must not be before its start time."
    InvalidNoteOwner ->
      "Note owner must be shared, left, or right."
    InvalidDiaryOwner ->
      "Diary owner must be left or right."

-- | Check that the title is non-blank and that the time range is valid
validateMemoryInputFields :: MemoryInput -> Either ValidationError ()
validateMemoryInputFields input = do
  _ <- requireNonBlank "title" (miTitle input)
  validateMemoryRange (miTimeFrom input) (miTimeTo input)

-- | Check that the title field is non-blank
validateImportantDayInputFields :: ImportantDayInput -> Either ValidationError ()
validateImportantDayInputFields input = do
  _ <- requireNonBlank "title" (idiTitle input)
  pure ()

-- | Check that owner, title and body are valid
validateNoteInputFields :: NoteInput -> Either ValidationError ()
validateNoteInputFields input = do
  let owner = T.toLower (T.strip (niOwner input))
  if owner `elem` ["shared", "left", "right"]
    then pure ()
    else Left InvalidNoteOwner
  _ <- requireNonBlank "title" (niTitle input)
  _ <- requireNonBlank "body" (niBody input)
  pure ()

-- | Check that the title is non-blank
validatePlanInputFields :: PlanInput -> Either ValidationError ()
validatePlanInputFields input = do
  _ <- requireNonBlank "title" (piTitle input)
  pure ()

-- | Check that a diary entry belongs to one profile and has non-blank content
validateDiaryInputFields :: DiaryInput -> Either ValidationError ()
validateDiaryInputFields input = do
  let owner = T.toLower (T.strip (deiOwner input))
  if owner `elem` ["left", "right"]
    then pure ()
    else Left InvalidDiaryOwner
  _ <- requireNonBlank "body" (deiBody input)
  pure ()

-- | Check that the rename target is non-blank
validateRenameInputFields :: RenameInput -> Either ValidationError ()
validateRenameInputFields input = do
  _ <- requireNonBlank "name" (riNewName input)
  pure ()

-- | Return 'Left InvalidTimeRange' if end is strictly before start
validateMemoryRange :: UTCTime -> UTCTime -> Either ValidationError ()
validateMemoryRange start end =
  if start <= end
    then Right ()
    else Left InvalidTimeRange

-- | Strip leading/trailing whitespace...fail with 'RequiredField' if the result is empty 
requireNonBlank :: Text -> Text -> Either ValidationError Text
requireNonBlank fieldName value =
  let trimmed = T.strip value
   in if T.null trimmed
        then Left (RequiredField fieldName)
        else Right trimmed

-- | Strip whitespace from an optional field and collapse a blank string to 'Nothing'
cleanMaybeText :: Maybe Text -> Maybe Text
cleanMaybeText =
  ( >>=
      \value ->
        let trimmed = T.strip value
         in if T.null trimmed then Nothing else Just trimmed
  )
