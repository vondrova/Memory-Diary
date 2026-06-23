{-# LANGUAGE OverloadedStrings #-}

-- | Pure validation rules shared by handlers.
module Domain.Validation
  ( ValidationError (..)
  , renderValidationError
  , requireNonBlank
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Required field was blank.
data ValidationError = RequiredField Text
  deriving (Eq, Show)

-- | Convert a 'ValidationError' into a user-facing message.
renderValidationError :: ValidationError -> Text
renderValidationError (RequiredField field) =
  "Missing required field: " <> field

-- | Strip whitespace and fail with 'RequiredField' if the result is empty.
requireNonBlank :: Text -> Text -> Either ValidationError Text
requireNonBlank fieldName value =
  let trimmed = T.strip value
  in if T.null trimmed
       then Left (RequiredField fieldName)
       else Right trimmed
