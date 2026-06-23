{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Library entry point for the Memory Diary backend.
module Lib
  ( startApp,
  )
where

import qualified Api
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, runMigration)
import Database.Persist.Sql (runSqlPool)
import Handler.Diary (diaryServer)
import Handler.Geocode (geocodeServer)
import Handler.ImportantDays (importantDayServer)
import Handler.Memories (memoryServer)
import Handler.Notes (noteServer)
import Handler.Photos (initPhotosDir, photosApp, uploadPhotos)
import Handler.Plans (planServer)
import Handler.Relationship (profileServer, relationshipServer)
import Handler.Stats (statsServer)
import Handler.Tags (locationServer, planCategoryServer, tagServer)
import Handler.Trash (trashServer)
import Models (migrateAll, syncCatalogValues)
import Network.HTTP.Types.Header (ResponseHeaders, hCacheControl, hExpires)
import Network.Wai (mapResponseHeaders)
import qualified Network.Wai as Wai
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import WaiAppStatic.Types (StaticSettings (..), unsafeToPiece)

type FullApi =
  Api.MemoryApi
    :<|> Api.ImportantDayApi
    :<|> Api.NoteApi
    :<|> Api.PlanApi
    :<|> Api.StatsApi
    :<|> Api.TrashApi
    :<|> Api.PhotoApi
    :<|> Api.GeocodeApi
    :<|> Api.TagApi
    :<|> Api.LocationApi
    :<|> Api.PlanCategoryApi
    :<|> Api.RelationshipApi
    :<|> Api.ProfileApi
    :<|> Api.DiaryApi
    :<|> "photos" :> Raw
    :<|> Raw

app :: String -> String -> ConnectionPool -> Wai.Application
app staticPath photosPath pool =
  serve
    (Proxy :: Proxy FullApi)
    ( hoistServer Api.memoryApi (`runReaderT` pool) memoryServer
        :<|> hoistServer Api.importantDayApi (`runReaderT` pool) importantDayServer
        :<|> hoistServer Api.noteApi (`runReaderT` pool) noteServer
        :<|> hoistServer Api.planApi (`runReaderT` pool) planServer
        :<|> hoistServer Api.statsApi (`runReaderT` pool) statsServer
        :<|> hoistServer Api.trashApi (`runReaderT` pool) trashServer
        :<|> uploadPhotos photosPath
        :<|> hoistServer Api.geocodeApi (`runReaderT` pool) geocodeServer
        :<|> hoistServer Api.tagApi (`runReaderT` pool) tagServer
        :<|> hoistServer Api.locationApi (`runReaderT` pool) locationServer
        :<|> hoistServer Api.planCategoryApi (`runReaderT` pool) planCategoryServer
        :<|> hoistServer Api.relationshipApi (`runReaderT` pool) relationshipServer
        :<|> hoistServer Api.profileApi (`runReaderT` pool) profileServer
        :<|> hoistServer Api.diaryApi (`runReaderT` pool) diaryServer
        :<|> Tagged (noCache (photosApp photosPath))
        :<|> Tagged (noCache (staticApp settings))
    )
  where
    settings =
      (defaultWebAppSettings staticPath)
        { ssIndices = [unsafeToPiece "index.html"]
        }

-- | Read runtime configuration, prepare the database and start the HTTP server.
startApp :: IO ()
startApp = do
  databaseUrl <- fromMaybe "postgresql://memory_diary:memory_diary@localhost:5432/memory_diary" <$> lookupEnv "DATABASE_URL"
  staticPath <- fromMaybe "../frontend" <$> lookupEnv "STATIC_PATH"
  photosPath <- fromMaybe "../photos" <$> lookupEnv "PHOTOS_PATH"
  port <- maybe 3000 (fromMaybe 3000 . readMaybe) <$> lookupEnv "PORT"
  initPhotosDir photosPath
  pool <- runNoLoggingT $ createPostgresqlPool (pack databaseUrl) 10
  runSqlPool (runMigration migrateAll) pool
  runSqlPool syncCatalogValues pool
  putStrLn $
    "Server starting on http://localhost:"
      <> show port
      <> " (static: "
      <> staticPath
      <> ", photos: "
      <> photosPath
      <> ")"
  run port (app staticPath photosPath pool)

noCache :: Wai.Application -> Wai.Application
noCache waiApp request sendResponse =
  waiApp request $ sendResponse . mapResponseHeaders replaceCacheHeaders

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
