{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Public HTTP API type declarations for all resources.
module Api
  ( MemoryApi,
    ImportantDayApi,
    NoteApi,
    PlanApi,
    StatsApi,
    TrashApi,
    PhotoApi,
    GeocodeApi,
    TagApi,
    LocationApi,
    PlanCategoryApi,
    RelationshipApi,
    ProfileApi,
    DiaryApi,
    memoryApi,
    importantDayApi,
    noteApi,
    planApi,
    statsApi,
    trashApi,
    photoApi,
    geocodeApi,
    tagApi,
    locationApi,
    planCategoryApi,
    relationshipApi,
    profileApi,
    diaryApi,
  )
where

import Data.Aeson (Value) -- for geocode API responses
import Data.Text (Text)
import Models (Stats) -- for the stats API response
import Servant
import Servant.Multipart (MultipartData, MultipartForm, Tmp) -- for photo uploads
import Types

-- API TYPES -------------------------------------------------------------------

-- | Memory CRUD with optional filtering by free-text query, tag, location, and date range
--   The on-this-day route returns memories whose anniversary falls on today's date
type MemoryApi =
  "api"
    :> "memories"
    :> QueryParam "q" Text -- free-text search across memory content
    :> QueryParam "tag" Text -- filter by tag
    :> QueryParam "location" Text -- filter by location
    :> QueryParam "from" Text -- filter by start of date range
    :> QueryParam "to" Text -- filter by end of date range
    :> Get '[JSON] [MemoryRow]
    :<|> "api" :> "memories" :> ReqBody '[JSON] MemoryInput :> Post '[JSON] MemoryRow -- create new memory
    :<|> "api" :> "memories" :> "on-this-day" :> Get '[JSON] [MemoryRow] -- get memories with an anniversary on today's date
    :<|> "api" :> "memories" :> Capture "id" Int :> Get '[JSON] MemoryRow -- get memory by id
    :<|> "api" :> "memories" :> Capture "id" Int :> ReqBody '[JSON] MemoryInput :> Put '[JSON] NoContent -- update memory by id
    :<|> "api" :> "memories" :> Capture "id" Int :> Delete '[JSON] NoContent -- delete memory by id (soft delete to trash)

-- | Aggregated statistics computed over all memories
type StatsApi =
  "api" :> "stats" :> Get '[JSON] Stats

-- | Photo upload via multipart/form-data...returns a list of stored filenames
type PhotoApi =
  "api" :> "photos" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] [Text]

-- | Important-day (birthday, anniversary, etc.) CRUD
type ImportantDayApi =
  "api" :> "important-days" :> Get '[JSON] [ImportantDayRow]
    :<|> "api" :> "important-days" :> ReqBody '[JSON] ImportantDayInput :> Post '[JSON] ImportantDayRow
    :<|> "api" :> "important-days" :> Capture "id" Int :> ReqBody '[JSON] ImportantDayInput :> Put '[JSON] NoContent
    :<|> "api" :> "important-days" :> Capture "id" Int :> Delete '[JSON] NoContent

-- | Couple notes CRUD — each note belongs to an owner ("left", "right", or "shared")
type NoteApi =
  "api" :> "notes" :> Get '[JSON] [NoteRow]
    :<|> "api" :> "notes" :> ReqBody '[JSON] NoteInput :> Post '[JSON] NoteRow
    :<|> "api" :> "notes" :> Capture "id" Int :> ReqBody '[JSON] NoteInput :> Put '[JSON] NoContent
    :<|> "api" :> "notes" :> Capture "id" Int :> Delete '[JSON] NoContent

-- | Couple plans CRUD — each plan belongs to a category and has a done flag
type PlanApi =
  "api" :> "plans" :> Get '[JSON] [PlanRow]
    :<|> "api" :> "plans" :> ReqBody '[JSON] PlanInput :> Post '[JSON] PlanRow
    :<|> "api" :> "plans" :> Capture "id" Int :> ReqBody '[JSON] PlanInput :> Put '[JSON] NoContent
    :<|> "api" :> "plans" :> Capture "id" Int :> Delete '[JSON] NoContent

-- | Soft-delete trash: list items, restore one, or permanently delete one
type TrashApi =
  "api" :> "trash" :> Get '[JSON] [TrashRow] -- list all items in the trash
    :<|> "api" :> "trash" :> Capture "id" Int :> "restore" :> Post '[JSON] NoContent -- restore a trashed item by id
    :<|> "api" :> "trash" :> Capture "id" Int :> Delete '[JSON] NoContent -- permanently delete a trashed item by id

-- | OpenStreetMap Nominatim proxy...forwards the query and returns place suggestions
type GeocodeApi =
  "api" :> "geocode" :> QueryParam "q" Text :> Get '[JSON] Value

-- | Bulk tag rename and delete across all memories
type TagApi =
  "api" :> "tags" :> Get '[JSON] [Text]
    :<|> "api" :> "tags" :> ReqBody '[JSON] CatalogInput :> Post '[JSON] NoContent
    :<|> "api" :> "tags" :> Capture "name" Text :> "rename" :> ReqBody '[JSON] RenameInput :> Put '[JSON] NoContent
    :<|> "api" :> "tags" :> Capture "name" Text :> Delete '[JSON] NoContent

-- | Bulk location rename and delete across all memories
type LocationApi =
  "api" :> "locations" :> Get '[JSON] [Text]
    :<|> "api" :> "locations" :> ReqBody '[JSON] CatalogInput :> Post '[JSON] NoContent
    :<|> "api" :> "locations" :> Capture "name" Text :> "rename" :> ReqBody '[JSON] RenameInput :> Put '[JSON] NoContent
    :<|> "api" :> "locations" :> Capture "name" Text :> Delete '[JSON] NoContent

-- | Bulk plan-category rename and delete (with optional scope: pending | done | all)
type PlanCategoryApi =
  "api" :> "plan-categories" :> Get '[JSON] [Text]
    :<|> "api" :> "plan-categories" :> ReqBody '[JSON] CatalogInput :> Post '[JSON] NoContent
    :<|> "api" :> "plan-categories" :> Capture "name" Text :> "rename" :> ReqBody '[JSON] RenameInput :> Put '[JSON] NoContent
    :<|> "api" :> "plan-categories" :> Capture "name" Text :> ReqBody '[JSON] DeleteCategoryInput :> Delete '[JSON] NoContent

-- | Relationship CRUD: start date, heart colour, and computed countdowns
type RelationshipApi =
  "api" :> "relationship" :> Get '[JSON] RelationshipRow
    :<|> "api" :> "relationship" :> ReqBody '[JSON] RelationshipInput :> Put '[JSON] RelationshipRow

-- | Person-profile CRUD for the left and right sides
type ProfileApi =
  "api" :> "profiles" :> Capture "side" Text :> Get '[JSON] ProfileRow
    :<|> "api" :> "profiles" :> Capture "side" Text :> ReqBody '[JSON] ProfileInput :> Put '[JSON] ProfileRow

-- | Personal diary entries for each partner, sorted newest first
type DiaryApi =
  "api" :> "diary" :> Get '[JSON] [DiaryRow]
    :<|> "api" :> "diary" :> ReqBody '[JSON] DiaryInput :> Post '[JSON] DiaryRow
    :<|> "api" :> "diary" :> Capture "id" Int :> ReqBody '[JSON] DiaryInput :> Put '[JSON] NoContent
    :<|> "api" :> "diary" :> Capture "id" Int :> Delete '[JSON] NoContent

-- PROXIES ---------------------------------------------------------------------

-- | Proxy values used by Servant to wire each API type to its handler server in Main.hs
memoryApi :: Proxy MemoryApi
memoryApi = Proxy

statsApi :: Proxy StatsApi
statsApi = Proxy

photoApi :: Proxy PhotoApi
photoApi = Proxy

importantDayApi :: Proxy ImportantDayApi
importantDayApi = Proxy

noteApi :: Proxy NoteApi
noteApi = Proxy

planApi :: Proxy PlanApi
planApi = Proxy

trashApi :: Proxy TrashApi
trashApi = Proxy

geocodeApi :: Proxy GeocodeApi
geocodeApi = Proxy

tagApi :: Proxy TagApi
tagApi = Proxy

locationApi :: Proxy LocationApi
locationApi = Proxy

planCategoryApi :: Proxy PlanCategoryApi
planCategoryApi = Proxy

relationshipApi :: Proxy RelationshipApi
relationshipApi = Proxy

profileApi :: Proxy ProfileApi
profileApi = Proxy

diaryApi :: Proxy DiaryApi
diaryApi = Proxy
