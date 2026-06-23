{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Models
  ( module Models
  , module Database.Persist
  , module Database.Persist.Postgresql
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, diffDays)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Domain.Recurrence (computeNextOccurrence, parseDay, formatDay)
import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MemoryDb
  title       Text
  timeFrom    Text
  timeTo      Text
  description Text Maybe
  location    Text Maybe
  tags        Text
  photos      Text
  deriving Show

ImportantDayDb
  title Text
  date  Text
  note  Text Maybe
  kind  Text
  deriving Show

RelationshipDb
  partner1  Text
  partner2  Text
  startDate Text
  note      Text Maybe
  deriving Show

CoupleNoteDb
  owner     Text
  title     Text
  body      Text
  createdAt Text
  deriving Show

CouplePlanDb
  category Text
  title    Text
  detail   Text Maybe
  done     Bool
  deriving Show

DiaryEntryDb
  date    Text
  mood    Text Maybe
  body    Text
  weather Text Maybe
  deriving Show
|]

splitComma :: Text -> [String]
splitComma t
  | T.null t  = []
  | otherwise = map T.unpack (T.splitOn "," t)

joinComma :: [String] -> Text
joinComma = T.intercalate "," . map T.pack

toMemory :: Entity MemoryDb -> Memory
toMemory (Entity key db) = Memory
  { memoryId          = fromIntegral (fromSqlKey key)
  , memoryTitle       = T.unpack (memoryDbTitle db)
  , memoryTimeFrom    = T.unpack (memoryDbTimeFrom db)
  , memoryTimeTo      = T.unpack (memoryDbTimeTo db)
  , memoryDescription = T.unpack <$> memoryDbDescription db
  , memoryLocation    = T.unpack <$> memoryDbLocation db
  , memoryTags        = splitComma (memoryDbTags db)
  , memoryPhotos      = splitComma (memoryDbPhotos db)
  }

toImportantDay :: Day -> Entity ImportantDayDb -> ImportantDay
toImportantDay today (Entity key db) =
  let (nextOcc, days, month, day) = computeNextOccurrence today (T.unpack (importantDayDbDate db))
  in ImportantDay
    { importantDayId             = Just (fromIntegral (fromSqlKey key))
    , importantDayTitle          = T.unpack (importantDayDbTitle db)
    , importantDayDate           = T.unpack (importantDayDbDate db)
    , importantDayNote           = T.unpack <$> importantDayDbNote db
    , importantDayKind           = T.unpack (importantDayDbKind db)
    , importantDayNextOccurrence = nextOcc
    , importantDayDaysUntil      = days
    , importantDayMonth          = month
    , importantDayDay            = day
    }

toRelationship :: Day -> Entity RelationshipDb -> Relationship
toRelationship today (Entity key db) =
  let startStr = T.unpack (relationshipDbStartDate db)
      daysTog  = maybe 0 (fromIntegral . diffDays today) (parseDay startStr)
      yearsTog = daysTog `div` 365
      (nextAnn, daysUntilAnn, _, _) = computeNextOccurrence today startStr
  in Relationship
    { relationshipId              = Just (fromIntegral (fromSqlKey key))
    , relationshipPartner1        = T.unpack (relationshipDbPartner1 db)
    , relationshipPartner2        = T.unpack (relationshipDbPartner2 db)
    , relationshipStartDate       = startStr
    , relationshipNote            = T.unpack <$> relationshipDbNote db
    , relationshipDaysTogether    = daysTog
    , relationshipYearsTogether   = yearsTog
    , relationshipNextAnniversary = nextAnn
    , relationshipDaysUntilAnniv  = daysUntilAnn
    }

toCoupleNote :: Entity CoupleNoteDb -> CoupleNote
toCoupleNote (Entity key db) = CoupleNote
  { coupleNoteId        = Just (fromIntegral (fromSqlKey key))
  , coupleNoteOwner     = T.unpack (coupleNoteDbOwner db)
  , coupleNoteTitle     = T.unpack (coupleNoteDbTitle db)
  , coupleNoteBody      = T.unpack (coupleNoteDbBody db)
  , coupleNoteCreatedAt = T.unpack (coupleNoteDbCreatedAt db)
  }

toCouplePlan :: Entity CouplePlanDb -> CouplePlan
toCouplePlan (Entity key db) = CouplePlan
  { couplePlanId       = Just (fromIntegral (fromSqlKey key))
  , couplePlanCategory = T.unpack (couplePlanDbCategory db)
  , couplePlanTitle    = T.unpack (couplePlanDbTitle db)
  , couplePlanDetail   = T.unpack <$> couplePlanDbDetail db
  , couplePlanDone     = couplePlanDbDone db
  }

toDiaryEntry :: Entity DiaryEntryDb -> DiaryEntry
toDiaryEntry (Entity key db) = DiaryEntry
  { diaryEntryId      = Just (fromIntegral (fromSqlKey key))
  , diaryEntryDate    = T.unpack (diaryEntryDbDate db)
  , diaryEntryMood    = T.unpack <$> diaryEntryDbMood db
  , diaryEntryBody    = T.unpack (diaryEntryDbBody db)
  , diaryEntryWeather = T.unpack <$> diaryEntryDbWeather db
  }
