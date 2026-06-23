{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types used across the application (Input = API request bodies, Row = API response rows)
module Types
  ( MemoryInput (..),
    MemoryRow (..),
    ImportantDayInput (..),
    ImportantDayRow (..),
    NoteInput (..),
    NoteRow (..),
    PlanInput (..),
    PlanRow (..),
    TrashRow (..),
    RenameInput (..),
    CatalogInput (..),
    DeleteCategoryInput (..),
    RelationshipInput (..),
    RelationshipRow (..),
    ProfileInput (..),
    ProfileRow (..),
    DiaryInput (..),
    DiaryRow (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data MemoryInput = MemoryInput
  { miTitle :: Text,
    miTimeFrom :: UTCTime,
    miTimeTo :: UTCTime,
    miDescription :: Maybe Text,
    miLocation :: Maybe Text,
    miTags :: [Text],
    miPhotos :: [Text]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON MemoryInput where
  parseJSON = withObject "MemoryInput" $ \o ->
    MemoryInput
      <$> o .: "title"
      <*> o .: "timeFrom"
      <*> o .: "timeTo"
      <*> o .: "description"
      <*> o .: "location"
      <*> o .: "tags"
      <*> o .: "photos"

instance ToJSON MemoryInput where
  toJSON r =
    object
      [ "title" .= miTitle r,
        "timeFrom" .= miTimeFrom r,
        "timeTo" .= miTimeTo r,
        "description" .= miDescription r,
        "location" .= miLocation r,
        "tags" .= miTags r,
        "photos" .= miPhotos r
      ]

data MemoryRow = MemoryRow
  { mrId :: Int,
    mrTitle :: Text,
    mrTimeFrom :: UTCTime,
    mrTimeTo :: UTCTime,
    mrDescription :: Maybe Text,
    mrLocation :: Maybe Text,
    mrTags :: [Text],
    mrPhotos :: [Text]
  }
  deriving stock (Eq, Show)

instance ToJSON MemoryRow where
  toJSON r =
    object
      [ "id" .= mrId r,
        "title" .= mrTitle r,
        "timeFrom" .= mrTimeFrom r,
        "timeTo" .= mrTimeTo r,
        "description" .= mrDescription r,
        "location" .= mrLocation r,
        "tags" .= mrTags r,
        "photos" .= mrPhotos r
      ]

data ImportantDayInput = ImportantDayInput
  { idiTitle :: Text,
    idiDate :: UTCTime,
    idiNote :: Maybe Text
  }
  deriving stock (Eq, Show)

instance FromJSON ImportantDayInput where
  parseJSON = withObject "ImportantDayInput" $ \o ->
    ImportantDayInput
      <$> (o .: "title" <|> o .: "name")
      <*> o .: "date"
      <*> o .: "note"

instance ToJSON ImportantDayInput where
  toJSON r =
    object
      [ "title" .= idiTitle r,
        "date" .= idiDate r,
        "note" .= idiNote r
      ]

data ImportantDayRow = ImportantDayRow
  { idrId :: Maybe Int,
    idrTitle :: Text,
    idrDate :: UTCTime,
    idrNote :: Maybe Text,
    idrKind :: Text,
    idrOwner :: Maybe Text,
    idrNextOccurrence :: UTCTime,
    idrDaysUntil :: Integer,
    idrMonth :: Int,
    idrDay :: Int
  }
  deriving stock (Eq, Show)

instance ToJSON ImportantDayRow where
  toJSON r =
    object
      [ "id" .= idrId r,
        "title" .= idrTitle r,
        "date" .= idrDate r,
        "note" .= idrNote r,
        "kind" .= idrKind r,
        "owner" .= idrOwner r,
        "nextOccurrence" .= idrNextOccurrence r,
        "daysUntil" .= idrDaysUntil r,
        "month" .= idrMonth r,
        "day" .= idrDay r
      ]

data NoteInput = NoteInput
  { niOwner :: Text,
    niTitle :: Text,
    niBody :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON NoteInput where
  parseJSON = withObject "NoteInput" $ \o ->
    NoteInput
      <$> o .: "owner"
      <*> o .: "title"
      <*> o .: "body"

instance ToJSON NoteInput where
  toJSON r =
    object
      [ "owner" .= niOwner r,
        "title" .= niTitle r,
        "body" .= niBody r
      ]

data NoteRow = NoteRow
  { nrId :: Int,
    nrOwner :: Text,
    nrTitle :: Text,
    nrBody :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON NoteRow where
  toJSON r =
    object
      [ "id" .= nrId r,
        "owner" .= nrOwner r,
        "title" .= nrTitle r,
        "body" .= nrBody r
      ]

data PlanInput = PlanInput
  { piCategory :: Text,
    piTitle :: Text,
    piDetail :: Maybe Text,
    piDone :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON PlanInput where
  parseJSON = withObject "PlanInput" $ \o ->
    PlanInput
      <$> o .: "category"
      <*> o .: "title"
      <*> o .: "detail"
      <*> o .: "done"

instance ToJSON PlanInput where
  toJSON r =
    object
      [ "category" .= piCategory r,
        "title" .= piTitle r,
        "detail" .= piDetail r,
        "done" .= piDone r
      ]

data PlanRow = PlanRow
  { prId :: Int,
    prCategory :: Text,
    prTitle :: Text,
    prDetail :: Maybe Text,
    prDone :: Bool
  }
  deriving stock (Eq, Show)

instance ToJSON PlanRow where
  toJSON r =
    object
      [ "id" .= prId r,
        "category" .= prCategory r,
        "title" .= prTitle r,
        "detail" .= prDetail r,
        "done" .= prDone r
      ]

-- | Body for tag and location rename operations
newtype RenameInput = RenameInput
  { riNewName :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON RenameInput where
  parseJSON = withObject "RenameInput" $ \o ->
    RenameInput <$> o .: "newName"

instance ToJSON RenameInput where
  toJSON r = object ["newName" .= riNewName r]

-- | Body for creating a persistent tag, location, or plan-category option
newtype CatalogInput = CatalogInput
  { ciName :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON CatalogInput where
  parseJSON = withObject "CatalogInput" $ \o ->
    CatalogInput <$> o .: "name"

instance ToJSON CatalogInput where
  toJSON r = object ["name" .= ciName r]

-- | Body for plan-category delete, allowing scope restriction (to let users preserve completed plans if they want)
newtype DeleteCategoryInput = DeleteCategoryInput
  { dciScope :: Maybe Text
  }
  deriving stock (Eq, Show)

instance FromJSON DeleteCategoryInput where
  parseJSON = withObject "DeleteCategoryInput" $ \o ->
    DeleteCategoryInput <$> o .:? "scope"

instance ToJSON DeleteCategoryInput where
  toJSON r = object ["scope" .= dciScope r]

data TrashRow = TrashRow
  { trId :: Int,
    trKind :: Text,
    trName :: Text,
    trDeletedAt :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON TrashRow where
  toJSON r =
    object
      [ "id" .= trId r,
        "kind" .= trKind r,
        "name" .= trName r,
        "deletedAt" .= trDeletedAt r
      ]

-- | Input for creating or updating a relationship record
data RelationshipInput = RelationshipInput
  { riStartDate :: Maybe UTCTime,
    riHeartColor :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON RelationshipInput where
  parseJSON = withObject "RelationshipInput" $ \o ->
    RelationshipInput
      <$> o .:? "startDate"
      <*> o .: "heartColor"

instance ToJSON RelationshipInput where
  toJSON r =
    object
      [ "startDate" .= riStartDate r,
        "heartColor" .= riHeartColor r
      ]

-- | Relationship data returned by GET /api/relationship, including computed countdown fields
data RelationshipRow = RelationshipRow
  { rrStartDate :: Maybe UTCTime,
    rrHeartColor :: Text,
    rrDaysTogether :: Maybe Int,
    rrNextAnniversary :: Maybe UTCTime,
    rrAnniversaryNumber :: Maybe Integer,
    rrNextMonthiversary :: Maybe UTCTime,
    rrMonthiversaryNum :: Maybe Integer,
    rrAnniversaryDays :: Maybe Integer,
    rrMonthiversaryDays :: Maybe Integer
  }
  deriving stock (Eq, Show)

instance ToJSON RelationshipRow where
  toJSON r =
    object
      [ "startDate" .= rrStartDate r,
        "heartColor" .= rrHeartColor r,
        "daysTogether" .= rrDaysTogether r,
        "nextAnniversary" .= rrNextAnniversary r,
        "anniversaryNumber" .= rrAnniversaryNumber r,
        "nextMonthiversary" .= rrNextMonthiversary r,
        "monthiversaryNum" .= rrMonthiversaryNum r,
        "anniversaryDays" .= rrAnniversaryDays r,
        "monthiversaryDays" .= rrMonthiversaryDays r
      ]

-- | Input for creating or updating a person profile
data ProfileInput = ProfileInput
  { piName :: Text,
    piDisplayMode :: Text,
    piPhoto :: Maybe Text,
    piFigureColor :: Text,
    piAccessory :: Text,
    piExpression :: Text,
    piBirthday :: Maybe UTCTime
  }
  deriving stock (Eq, Show)

instance FromJSON ProfileInput where
  parseJSON = withObject "ProfileInput" $ \o ->
    ProfileInput
      <$> o .: "name"
      <*> o .: "displayMode"
      <*> o .:? "photo"
      <*> o .: "figureColor"
      <*> o .: "accessory"
      <*> o .: "expression"
      <*> o .:? "birthday"

instance ToJSON ProfileInput where
  toJSON p =
    object
      [ "name" .= piName p,
        "displayMode" .= piDisplayMode p,
        "photo" .= piPhoto p,
        "figureColor" .= piFigureColor p,
        "accessory" .= piAccessory p,
        "expression" .= piExpression p,
        "birthday" .= piBirthday p
      ]

data DiaryInput = DiaryInput
  { deiOwner :: Text, -- "left" or "right"
    deiDate :: UTCTime,
    deiBody :: Text
  }
  deriving stock (Eq, Show)

-- | Parse DiaryInput from JSON request bodies for POST and PUT /api/diary
instance FromJSON DiaryInput where
  parseJSON = withObject "DiaryInput" $ \o ->
    DiaryInput
      <$> o .: "owner"
      <*> o .: "date"
      <*> o .: "body"

instance ToJSON DiaryInput where
  toJSON r =
    object
      [ "owner" .= deiOwner r,
        "date" .= deiDate r,
        "body" .= deiBody r
      ]

data DiaryRow = DiaryRow
  { derId :: Int,
    derOwner :: Text,
    derDate :: UTCTime,
    derBody :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON DiaryRow where
  toJSON r =
    object
      [ "id" .= derId r,
        "owner" .= derOwner r,
        "date" .= derDate r,
        "body" .= derBody r
      ]

-- | Person profile data returned by GET /api/profiles/:side
data ProfileRow = ProfileRow
  { prSide :: Text,
    prName :: Text,
    prDisplayMode :: Text,
    prPhoto :: Maybe Text,
    prFigureColor :: Text,
    prAccessory :: Text,
    prExpression :: Text,
    prBirthday :: Maybe UTCTime
  }
  deriving stock (Eq, Show)

instance ToJSON ProfileRow where
  toJSON p =
    object
      [ "side" .= prSide p,
        "name" .= prName p,
        "displayMode" .= prDisplayMode p,
        "photo" .= prPhoto p,
        "figureColor" .= prFigureColor p,
        "accessory" .= prAccessory p,
        "expression" .= prExpression p,
        "birthday" .= prBirthday p
      ]
