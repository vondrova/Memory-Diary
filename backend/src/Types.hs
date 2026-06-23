{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Memory (..)
  , MemoryInput (..)
  , ImportantDay (..)
  , ImportantDayInput (..)
  , Relationship (..)
  , RelationshipInput (..)
  , OnThisDay (..)
  , Stats (..)
  , CoupleNote (..)
  , CoupleNoteInput (..)
  , CouplePlan (..)
  , CouplePlanInput (..)
  , DiaryEntry (..)
  , DiaryEntryInput (..)
  , GeoResult (..)
  , aesonOpts
  ) where

import Data.Aeson
  ( FromJSON (..), ToJSON (..), Options, defaultOptions
  , fieldLabelModifier, genericToJSON, genericParseJSON
  , object, withObject, (.=), (.:)
  )
import Data.Char (toLower)
import Data.List (stripPrefix)
import GHC.Generics (Generic)

aesonOpts :: String -> Options
aesonOpts prefix = defaultOptions
  { fieldLabelModifier = \s -> case stripPrefix prefix s of
      Just (c : cs) -> toLower c : cs
      _             -> s
  }

data Memory = Memory
  { memoryId          :: Int
  , memoryTitle       :: String
  , memoryTimeFrom    :: String
  , memoryTimeTo      :: String
  , memoryDescription :: Maybe String
  , memoryLocation    :: Maybe String
  , memoryTags        :: [String]
  , memoryPhotos      :: [String]
  } deriving (Eq, Show, Generic)

instance FromJSON Memory where parseJSON = genericParseJSON (aesonOpts "memory")
instance ToJSON   Memory where toJSON    = genericToJSON   (aesonOpts "memory")

data MemoryInput = MemoryInput
  { inputTitle       :: String
  , inputTimeFrom    :: String
  , inputTimeTo      :: String
  , inputDescription :: Maybe String
  , inputLocation    :: Maybe String
  , inputTags        :: [String]
  , inputPhotos      :: [String]
  } deriving (Eq, Show, Generic)

instance FromJSON MemoryInput where parseJSON = genericParseJSON (aesonOpts "input")
instance ToJSON   MemoryInput where toJSON    = genericToJSON   (aesonOpts "input")

data ImportantDay = ImportantDay
  { importantDayId             :: Maybe Int
  , importantDayTitle          :: String
  , importantDayDate           :: String
  , importantDayNote           :: Maybe String
  , importantDayKind           :: String
  , importantDayNextOccurrence :: String
  , importantDayDaysUntil      :: Int
  , importantDayMonth          :: Int
  , importantDayDay            :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON ImportantDay where parseJSON = genericParseJSON (aesonOpts "importantDay")
instance ToJSON   ImportantDay where toJSON    = genericToJSON   (aesonOpts "importantDay")

data ImportantDayInput = ImportantDayInput
  { importantDayInputTitle :: String
  , importantDayInputDate  :: String
  , importantDayInputNote  :: Maybe String
  , importantDayInputKind  :: String
  } deriving (Eq, Show, Generic)

instance FromJSON ImportantDayInput where parseJSON = genericParseJSON (aesonOpts "importantDayInput")
instance ToJSON   ImportantDayInput where toJSON    = genericToJSON   (aesonOpts "importantDayInput")

data Relationship = Relationship
  { relationshipId              :: Maybe Int
  , relationshipPartner1        :: String
  , relationshipPartner2        :: String
  , relationshipStartDate       :: String
  , relationshipNote            :: Maybe String
  , relationshipDaysTogether    :: Int
  , relationshipYearsTogether   :: Int
  , relationshipNextAnniversary :: String
  , relationshipDaysUntilAnniv  :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Relationship where parseJSON = genericParseJSON (aesonOpts "relationship")
instance ToJSON   Relationship where toJSON    = genericToJSON   (aesonOpts "relationship")

data RelationshipInput = RelationshipInput
  { relationshipInputPartner1  :: String
  , relationshipInputPartner2  :: String
  , relationshipInputStartDate :: String
  , relationshipInputNote      :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromJSON RelationshipInput where parseJSON = genericParseJSON (aesonOpts "relationshipInput")
instance ToJSON   RelationshipInput where toJSON    = genericToJSON   (aesonOpts "relationshipInput")

data OnThisDay = OnThisDay
  { onThisDayMemories      :: [Memory]
  , onThisDayImportantDays :: [ImportantDay]
  , onThisDayIsAnniversary :: Bool
  , onThisDayYears         :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON OnThisDay where parseJSON = genericParseJSON (aesonOpts "onThisDay")
instance ToJSON   OnThisDay where toJSON    = genericToJSON   (aesonOpts "onThisDay")

data Stats = Stats
  { statsTotalMinutes    :: Int
  , statsMemoryCount     :: Int
  , statsPhotoCount      :: Int
  , statsVisitedPlaces   :: Int
  , statsAverageMinutes  :: Int
  , statsLongestTitle    :: Maybe String
  , statsLongestMinutes  :: Int
  , statsTopTags         :: [(String, Int)]
  , statsTopLocations    :: [(String, Int)]
  , statsMonthlyActivity :: [(String, Int)]
  , statsDailyActivity   :: [(String, Int)]
  } deriving (Show, Generic)

instance FromJSON Stats where parseJSON = genericParseJSON (aesonOpts "stats")
instance ToJSON   Stats where toJSON    = genericToJSON   (aesonOpts "stats")

data CoupleNote = CoupleNote
  { coupleNoteId        :: Maybe Int
  , coupleNoteOwner     :: String
  , coupleNoteTitle     :: String
  , coupleNoteBody      :: String
  , coupleNoteCreatedAt :: String
  } deriving (Eq, Show, Generic)

instance FromJSON CoupleNote where parseJSON = genericParseJSON (aesonOpts "coupleNote")
instance ToJSON   CoupleNote where toJSON    = genericToJSON   (aesonOpts "coupleNote")

data CoupleNoteInput = CoupleNoteInput
  { coupleNoteInputOwner :: String
  , coupleNoteInputTitle :: String
  , coupleNoteInputBody  :: String
  } deriving (Eq, Show, Generic)

instance FromJSON CoupleNoteInput where parseJSON = genericParseJSON (aesonOpts "coupleNoteInput")
instance ToJSON   CoupleNoteInput where toJSON    = genericToJSON   (aesonOpts "coupleNoteInput")

data CouplePlan = CouplePlan
  { couplePlanId       :: Maybe Int
  , couplePlanCategory :: String
  , couplePlanTitle    :: String
  , couplePlanDetail   :: Maybe String
  , couplePlanDone     :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON CouplePlan where parseJSON = genericParseJSON (aesonOpts "couplePlan")
instance ToJSON   CouplePlan where toJSON    = genericToJSON   (aesonOpts "couplePlan")

data CouplePlanInput = CouplePlanInput
  { couplePlanInputCategory :: String
  , couplePlanInputTitle    :: String
  , couplePlanInputDetail   :: Maybe String
  , couplePlanInputDone     :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON CouplePlanInput where parseJSON = genericParseJSON (aesonOpts "couplePlanInput")
instance ToJSON   CouplePlanInput where toJSON    = genericToJSON   (aesonOpts "couplePlanInput")

data DiaryEntry = DiaryEntry
  { diaryEntryId      :: Maybe Int
  , diaryEntryDate    :: String
  , diaryEntryMood    :: Maybe String
  , diaryEntryBody    :: String
  , diaryEntryWeather :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromJSON DiaryEntry where parseJSON = genericParseJSON (aesonOpts "diaryEntry")
instance ToJSON   DiaryEntry where toJSON    = genericToJSON   (aesonOpts "diaryEntry")

data DiaryEntryInput = DiaryEntryInput
  { diaryEntryInputDate    :: String
  , diaryEntryInputMood    :: Maybe String
  , diaryEntryInputBody    :: String
  , diaryEntryInputWeather :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromJSON DiaryEntryInput where parseJSON = genericParseJSON (aesonOpts "diaryEntryInput")
instance ToJSON   DiaryEntryInput where toJSON    = genericToJSON   (aesonOpts "diaryEntryInput")

data GeoResult = GeoResult
  { geoDisplayName :: String
  , geoLat         :: String
  , geoLon         :: String
  } deriving (Show, Generic)

instance ToJSON GeoResult where
  toJSON gr = object
    [ "displayName" .= geoDisplayName gr
    , "lat"         .= geoLat gr
    , "lon"         .= geoLon gr
    ]

instance FromJSON GeoResult where
  parseJSON = withObject "GeoResult" $ \o ->
    GeoResult <$> o .: "display_name" <*> o .: "lat" <*> o .: "lon"
