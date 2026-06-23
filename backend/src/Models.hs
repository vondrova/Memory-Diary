{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Database schema for Memory Diary.
--   All tables are defined using Persistent's Template Haskell DSL (`persistLowerCase`).
--   'Stats' is not stored in the database, it is computed ad-hoc by aggregating memory data in 'Handler.Stats' / 'Domain.Aggregation' and returned via the API
module Models
  ( module Models,
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Sql (SqlPersistT, rawExecute)
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"] -- generate Persistent entities and a migration function to synchronize the database schema
  [persistLowerCase|

-- Memory table
MemoryDb json sql=memories
    title       Text
    timeFrom    UTCTime
    timeTo      UTCTime
    description Text Maybe
    location    Text Maybe
    deriving Show Eq

-- Tags table (M:N with memories)
MemoryTagDb json sql=memory_tags
    memoryId    MemoryDbId
    tag         Text
    UniqueMemoryTag memoryId tag
    deriving Show Eq

-- Photos table (1:N with memories)
MemoryPhotoDb json sql=memory_photos
    memoryId    MemoryDbId
    filename    Text
    position    Int
    deriving Show Eq

-- Catalog tables for tags and locations
TagCatalogDb json sql=tag_catalog
    name Text
    UniqueTagCatalog name
    deriving Show Eq
LocationCatalogDb json sql=location_catalog
    name Text
    UniqueLocationCatalog name
    deriving Show Eq

-- Important days: birthdays and manually added anniversaries
ImportantDayDb json sql=important_days
    title       Text
    date        UTCTime
    note        Text Maybe
    kind        Text
    deriving Show Eq

-- Relationship table for the couple's shared info 
RelationshipDb json sql=relationship
    key         Text
    startDate   UTCTime Maybe
    heartColor  Text
    UniqueRelationshipKey key
    deriving Show Eq

-- Person-profile table for each partner
PersonProfileDb json sql=person_profiles
    side        Text
    name        Text
    displayMode Text
    photo       Text Maybe
    figureColor Text
    accessory   Text
    expression  Text
    birthday    UTCTime Maybe
    UniquePersonSide side
    deriving Show Eq

-- Couple notes created by either partner
CoupleNoteDb json sql=couple_notes
    owner       Text
    title       Text
    body        Text
    UniqueNoteTitle owner title
    deriving Show Eq

-- Couple plans created by either partner
CouplePlanDb json sql=couple_plans
    category    Text
    title       Text
    detail      Text Maybe
    done        Bool
    deriving Show Eq

-- Catalog table for plan categories
PlanCategoryDb json sql=plan_categories
    name Text
    UniquePlanCategory name
    deriving Show Eq

-- Soft-deleted items from any table
TrashItemDb json sql=trash_items
    kind        Text
    payload     Text
    deletedAt   UTCTime
    deriving Show Eq

-- Personal diary entries
DiaryEntryDb json sql=diary_entries
    owner    Text       
    date     UTCTime    
    body     Text        
    deriving Show Eq
|]

-- | Backfill the tag and location catalogs from existing memory data (no-op if already up to date)
syncCatalogValues :: SqlPersistT IO ()
syncCatalogValues = do
  rawExecute "INSERT INTO tag_catalog (name) SELECT DISTINCT tag FROM memory_tags ON CONFLICT (name) DO NOTHING" []
  rawExecute "INSERT INTO location_catalog (name) SELECT DISTINCT location FROM memories WHERE location IS NOT NULL ON CONFLICT (name) DO NOTHING" []
  rawExecute "INSERT INTO plan_categories (name) SELECT DISTINCT category FROM couple_plans ON CONFLICT (name) DO NOTHING" []
  rawExecute "INSERT INTO plan_categories (name) VALUES ('Bucket list'), ('Places to eat'), ('Movies to watch'), ('Trips') ON CONFLICT (name) DO NOTHING" []

-- | Aggregate statistics returned by the stats endpoint
data Stats = Stats
  { statsTotalMinutes :: Int,
    statsMemoryCount :: Int,
    statsPhotoCount :: Int,
    statsVisitedPlaces :: Int,
    statsAverageMinutes :: Int,
    statsLongestTitle :: Maybe Text,
    statsLongestMinutes :: Int,
    statsTopTags :: [(Text, Int)],
    statsTopLocations :: [(Text, Int)],
    statsMonthlyActivity :: [(Text, Int)],
    statsDailyActivity :: [(Text, Int)] -- for heatmap
  }
  deriving stock (Eq, Show)

instance ToJSON Stats where
  toJSON s =
    object
      [ "totalMinutes" .= statsTotalMinutes s,
        "memoryCount" .= statsMemoryCount s,
        "photoCount" .= statsPhotoCount s,
        "visitedPlaces" .= statsVisitedPlaces s,
        "averageMinutes" .= statsAverageMinutes s,
        "longestTitle" .= statsLongestTitle s,
        "longestMinutes" .= statsLongestMinutes s,
        "topTags" .= statsTopTags s,
        "topLocations" .= statsTopLocations s,
        "monthlyActivity" .= statsMonthlyActivity s,
        "dailyActivity" .= statsDailyActivity s
      ]
