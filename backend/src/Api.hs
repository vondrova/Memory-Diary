{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API type declarations for all resources.
module Api
  ( MemoryAPI
  , ImportantDayAPI
  , RelationshipAPI
  , OnThisDayAPI
  , NoteAPI
  , PlanAPI
  , DiaryAPI
  , StatsAPI
  , GeoAPI
  , PhotoAPI
  , FullAPI
  ) where

import Data.Text (Text)
import Servant
import Servant.Multipart (MultipartData, MultipartForm, Tmp)
import Types

-- | Memory CRUD with optional filtering by query, tag, location, and date range.
type MemoryAPI =
       "api" :> "memories"
         :> QueryParam "q"        Text
         :> QueryParam "tag"      Text
         :> QueryParam "location" Text
         :> QueryParam "from"     Text
         :> QueryParam "to"       Text
         :> Get '[JSON] [Memory]
  :<|> "api" :> "memories" :> ReqBody '[JSON] MemoryInput :> Post '[JSON] Memory
  :<|> "api" :> "memories" :> Capture "id" Int :> ReqBody '[JSON] MemoryInput :> Put '[JSON] Memory
  :<|> "api" :> "memories" :> Capture "id" Int :> Delete '[JSON] NoContent

-- | Important-day CRUD.
type ImportantDayAPI =
       "api" :> "important-days" :> Get '[JSON] [ImportantDay]
  :<|> "api" :> "important-days" :> ReqBody '[JSON] ImportantDayInput :> Post '[JSON] ImportantDay
  :<|> "api" :> "important-days" :> Capture "id" Int :> ReqBody '[JSON] ImportantDayInput :> Put '[JSON] ImportantDay
  :<|> "api" :> "important-days" :> Capture "id" Int :> Delete '[JSON] NoContent

-- | Relationship upsert (there is at most one relationship record).
type RelationshipAPI =
       "api" :> "relationship" :> Get '[JSON] (Maybe Relationship)
  :<|> "api" :> "relationship" :> ReqBody '[JSON] RelationshipInput :> Put '[JSON] Relationship

-- | Memories and important days whose anniversary falls on today's date.
type OnThisDayAPI = "api" :> "on-this-day" :> Get '[JSON] OnThisDay

-- | Couple notes CRUD.
type NoteAPI =
       "api" :> "notes" :> Get '[JSON] [CoupleNote]
  :<|> "api" :> "notes" :> ReqBody '[JSON] CoupleNoteInput :> Post '[JSON] CoupleNote
  :<|> "api" :> "notes" :> Capture "id" Int :> ReqBody '[JSON] CoupleNoteInput :> Put '[JSON] CoupleNote
  :<|> "api" :> "notes" :> Capture "id" Int :> Delete '[JSON] NoContent

-- | Couple plans CRUD.
type PlanAPI =
       "api" :> "plans" :> Get '[JSON] [CouplePlan]
  :<|> "api" :> "plans" :> ReqBody '[JSON] CouplePlanInput :> Post '[JSON] CouplePlan
  :<|> "api" :> "plans" :> Capture "id" Int :> ReqBody '[JSON] CouplePlanInput :> Put '[JSON] CouplePlan
  :<|> "api" :> "plans" :> Capture "id" Int :> Delete '[JSON] NoContent

-- | Diary entry CRUD.
type DiaryAPI =
       "api" :> "diary" :> Get '[JSON] [DiaryEntry]
  :<|> "api" :> "diary" :> ReqBody '[JSON] DiaryEntryInput :> Post '[JSON] DiaryEntry
  :<|> "api" :> "diary" :> Capture "id" Int :> ReqBody '[JSON] DiaryEntryInput :> Put '[JSON] DiaryEntry
  :<|> "api" :> "diary" :> Capture "id" Int :> Delete '[JSON] NoContent

-- | Aggregated statistics computed over all memories.
type StatsAPI = "api" :> "stats" :> Get '[JSON] Stats

-- | OpenStreetMap Nominatim geocoding proxy.
type GeoAPI = "api" :> "geocode" :> QueryParam "q" Text :> Get '[JSON] [GeoResult]

-- | Photo upload via multipart/form-data.
type PhotoAPI = "api" :> "photos" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] [String]

-- | Combined API served by the application.
type FullAPI =
       MemoryAPI
  :<|> ImportantDayAPI
  :<|> RelationshipAPI
  :<|> OnThisDayAPI
  :<|> NoteAPI
  :<|> PlanAPI
  :<|> DiaryAPI
  :<|> StatsAPI
  :<|> GeoAPI
  :<|> PhotoAPI
  :<|> "photos" :> Raw
  :<|> Raw
