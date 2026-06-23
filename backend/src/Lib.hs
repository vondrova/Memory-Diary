{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Application entry point.
--   All API types live in "Api", all handlers in "Handler.*".
--   This module wires them together and re-exports names used by the test suite.
module Lib
  ( startApp,
    Memory (..),
    Stats (..),
    durMins,
    topN,
    splitComma,
    joinComma,
    computeNextOccurrence,
    parseDay,
    monthDay,
    computeStats,
  )
where

import Api
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, runMigration)
import Database.Persist.Sql (runSqlPool)
import Domain.Aggregation (computeStats, durMins, topN)
import Domain.Recurrence (computeNextOccurrence, monthDay, parseDay)
import Handler.Diary (diaryServer)
import Handler.Geocode (geocodeHandler)
import Handler.ImportantDays (importantDayServer)
import Handler.Memories (memoryServer)
import Handler.Notes (noteServer)
import Handler.Photos (photosApp, uploadPhotos)
import Handler.Plans (planServer)
import Handler.Relationship (getOnThisDay, relationshipServer)
import Handler.Stats (getStats)
import Models (joinComma, migrateAll, splitComma)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import Types

-- | Build the Servant WAI application, wiring each sub-API to its handler server.
app :: String -> String -> ConnectionPool -> Application
app staticPath photosPath pool =
  serve (Proxy :: Proxy FullAPI) $
    hoistServer (Proxy :: Proxy MemoryAPI) (`runReaderT` pool) memoryServer
      :<|> hoistServer (Proxy :: Proxy ImportantDayAPI) (`runReaderT` pool) importantDayServer
      :<|> hoistServer (Proxy :: Proxy RelationshipAPI) (`runReaderT` pool) relationshipServer
      :<|> hoistServer (Proxy :: Proxy OnThisDayAPI) (`runReaderT` pool) getOnThisDay
      :<|> hoistServer (Proxy :: Proxy NoteAPI) (`runReaderT` pool) noteServer
      :<|> hoistServer (Proxy :: Proxy PlanAPI) (`runReaderT` pool) planServer
      :<|> hoistServer (Proxy :: Proxy DiaryAPI) (`runReaderT` pool) diaryServer
      :<|> hoistServer (Proxy :: Proxy StatsAPI) (`runReaderT` pool) getStats
      :<|> hoistServer (Proxy :: Proxy GeoAPI) (`runReaderT` pool) geocodeHandler
      :<|> uploadPhotos photosPath
      :<|> Tagged (photosApp photosPath)
      :<|> Tagged (staticApp (defaultWebAppSettings staticPath))

-- | Start the application: read environment, run migrations, start Warp.
startApp :: IO ()
startApp = do
  dbUrl <- fromMaybe "postgresql://memory_diary:memory_diary@localhost:5432/memory_diary" <$> lookupEnv "DATABASE_URL"
  staticPath <- fromMaybe "../frontend" <$> lookupEnv "STATIC_PATH"
  photosPath <- fromMaybe "../photos" <$> lookupEnv "PHOTOS_PATH"
  pool <- runNoLoggingT $ createPostgresqlPool (pack dbUrl) 10
  runSqlPool (runMigration migrateAll) pool
  createDirectoryIfMissing True photosPath
  putStrLn "Memory Diary starting on port 3000..."
  run 3000 (app staticPath photosPath pool)
