{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE OverloadedStrings #-}  
{-# LANGUAGE TypeOperators #-}

module Main (main) where

-- Servant API type aliases and Proxy values for each sub-API, imported from Api.hs
import Api
  ( DiaryApi,
    GeocodeApi,
    ImportantDayApi,
    LocationApi,
    MemoryApi,
    NoteApi,
    PhotoApi,
    PlanApi,
    PlanCategoryApi,
    ProfileApi,
    RelationshipApi,
    StatsApi,
    TagApi,
    TrashApi,
    diaryApi,
    geocodeApi,
    importantDayApi,
    locationApi,
    memoryApi,
    noteApi,
    planApi,
    planCategoryApi,
    profileApi,
    relationshipApi,
    statsApi,
    tagApi,
    trashApi,
  )


-- PostgreSQL connection pool and schema migration runner from the Persistent library
import Database.Persist.Postgresql (createPostgresqlPool, runMigration)
import Database.Persist.Sql (runSqlPool)

-- Suppress migration log noise
import Control.Monad.Logger (runNoLoggingT)
-- Inject the DB pool into each request handler
import Control.Monad.Reader (runReaderT)

-- Converts the DATABASE_URL string to ByteString
import Data.ByteString.Char8 (pack)
-- supplies defaults for missing env vars
import Data.Maybe (fromMaybe)

-- Relationship and profile handlers (partner setup, profile config)
import Handler.Relationship (profileServer, relationshipServer)

-- Database schema and catalog sync functions
import Models (migrateAll, syncCatalogValues)

-- WAI and Warp for HTTP server, static file serving, and middleware
import Network.HTTP.Types.Header (ResponseHeaders, hCacheControl, hExpires)
import Network.Wai (mapResponseHeaders)
import qualified Network.Wai as Wai
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Servant

-- HTTP handler implementations, imported directly from each feature module
import Handler.Diary (diaryServer)
import Handler.Geocode (geocodeServer)
import Handler.ImportantDays (importantDayServer)
import Handler.Memories (memoryServer)
import Handler.Notes (noteServer)
import Handler.Photos (initPhotosDir, photosApp, uploadPhotos)
import Handler.Plans (planServer)
import Handler.Stats (statsServer)
import Handler.Tags (locationServer, planCategoryServer, tagServer)
import Handler.Trash (trashServer)

-- Reads DATABASE_URL, STATIC_PATH, PHOTOS_PATH and PORT from the process env
import System.Environment (lookupEnv)

-- Parses the PORT env var string to Int without throwing exceptions on invalid input
import Text.Read (readMaybe)

-- Lets us set index.html as the default file for directory requests
import WaiAppStatic.Types (StaticSettings (..), unsafeToPiece)

-- All routes served by this process; Servant checks handler types against this at compile time
type FullApi =
  MemoryApi
    :<|> ImportantDayApi
    :<|> NoteApi
    :<|> PlanApi
    :<|> StatsApi
    :<|> TrashApi
    :<|> PhotoApi
    :<|> GeocodeApi
    :<|> TagApi
    :<|> LocationApi
    :<|> PlanCategoryApi
    :<|> RelationshipApi
    :<|> ProfileApi
    :<|> DiaryApi
    :<|> "photos" :> Raw
    :<|> Raw


main :: IO ()
main = do
    -- Read env vars with defaults: Postgres URL, static file path, photos path, and server port
  databaseUrl <- fromMaybe "postgresql://memory_diary:memory_diary@localhost:5432/memory_diary" <$> lookupEnv "DATABASE_URL"
  staticPath <- fromMaybe "../frontend" <$> lookupEnv "STATIC_PATH"
  photosPath <- fromMaybe "../photos" <$> lookupEnv "PHOTOS_PATH"
  port <- maybe 3000 (fromMaybe 3000 . readMaybe) <$> lookupEnv "PORT"
  -- Ensure the photos directory exists
  initPhotosDir photosPath
  -- Create a PostgreSQL connection pool with no logging
  pool <- runNoLoggingT $ createPostgresqlPool (pack databaseUrl) 10
  -- Auto-generate any missing tables or columns declared in Models.hs
  runSqlPool (runMigration migrateAll) pool
  -- Backfill the tag/location catalog from existing memory data
  runSqlPool syncCatalogValues pool

  -- Build the Servant application by wiring each sub-API to its handler server via 'hoistServer'
  let 
      settings =
        (defaultWebAppSettings staticPath)
          { ssIndices = [unsafeToPiece "index.html"]
          }
      app =
        serve
          (Proxy :: Proxy FullApi)
          ( hoistServer memoryApi (`runReaderT` pool) memoryServer
              :<|> hoistServer importantDayApi (`runReaderT` pool) importantDayServer
              :<|> hoistServer noteApi (`runReaderT` pool) noteServer
              :<|> hoistServer planApi (`runReaderT` pool) planServer
              :<|> hoistServer statsApi (`runReaderT` pool) statsServer
              :<|> hoistServer trashApi (`runReaderT` pool) trashServer
              :<|> uploadPhotos photosPath
              :<|> hoistServer geocodeApi (`runReaderT` pool) geocodeServer
              :<|> hoistServer tagApi (`runReaderT` pool) tagServer
              :<|> hoistServer locationApi (`runReaderT` pool) locationServer
              :<|> hoistServer planCategoryApi (`runReaderT` pool) planCategoryServer
              :<|> hoistServer relationshipApi (`runReaderT` pool) relationshipServer
              :<|> hoistServer profileApi (`runReaderT` pool) profileServer
              :<|> hoistServer diaryApi (`runReaderT` pool) diaryServer
              :<|> Tagged (noCache (photosApp photosPath)) -- dont need pool, only path to the photo directory
              :<|> Tagged (noCache (staticApp settings))
          )
  putStrLn $
    "Server starting on http://localhost:"
      <> show port
      <> " (static: "
      <> staticPath
      <> ", photos: "
      <> photosPath
      <> ")"
  -- Start the Warp server on the configured port
  run port app

-- Functions to strip cache headers from static and photo responses, ensuring clients always get the latest version after updates (from StackOverflow: https://stackoverflow.com/questions/26692512/in-haskell-wai-how-do-i-add-headers-as-middleware)
noCache :: Wai.Application -> Wai.Application
noCache app req respond =
  app req $ respond . mapResponseHeaders replaceCacheHeaders

replaceCacheHeaders :: ResponseHeaders -> ResponseHeaders
replaceCacheHeaders headers =
  let filtered =
        filter
          ( \(name, _) ->
              name /= hCacheControl && name /= hExpires && name /= "Pragma"
          )
          headers
   in ("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")
        : ("Pragma", "no-cache")
        : ("Expires", "0")
        : filtered