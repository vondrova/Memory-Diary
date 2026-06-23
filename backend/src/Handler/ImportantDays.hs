{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the /api/important-days endpoints.
module Handler.ImportantDays
  ( importantDayServer
  ) where

import Api (ImportantDayAPI)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime, utctDay)
import Handler.Helpers (AppM, runDB)
import Models
import Servant
import Types
import qualified Data.Text as T

importantDayServer :: ServerT ImportantDayAPI AppM
importantDayServer =
       getImportantDays
  :<|> createImportantDay
  :<|> updateImportantDay
  :<|> deleteImportantDay

-- | Return all important days sorted by date, with recurrence projection.
getImportantDays :: AppM [ImportantDay]
getImportantDays = do
  today    <- liftIO $ utctDay <$> getCurrentTime
  entities <- runDB $ selectList [] [Asc ImportantDayDbDate]
  return $ map (toImportantDay today) entities

-- | Create a new important day.
createImportantDay :: ImportantDayInput -> AppM ImportantDay
createImportantDay input = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let db = ImportantDayDb
        { importantDayDbTitle = T.pack (importantDayInputTitle input)
        , importantDayDbDate  = T.pack (importantDayInputDate input)
        , importantDayDbNote  = T.pack <$> importantDayInputNote input
        , importantDayDbKind  = T.pack (importantDayInputKind input)
        }
  key <- runDB $ insert db
  return (toImportantDay today (Entity key db))

-- | Replace all fields of an existing important day.
updateImportantDay :: Int -> ImportantDayInput -> AppM ImportantDay
updateImportantDay dayId input = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let key = toSqlKey (fromIntegral dayId) :: Key ImportantDayDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> do
      let db = ImportantDayDb
            { importantDayDbTitle = T.pack (importantDayInputTitle input)
            , importantDayDbDate  = T.pack (importantDayInputDate input)
            , importantDayDbNote  = T.pack <$> importantDayInputNote input
            , importantDayDbKind  = T.pack (importantDayInputKind input)
            }
      runDB $ replace key db
      return (toImportantDay today (Entity key db))

-- | Delete an important day by id.
deleteImportantDay :: Int -> AppM NoContent
deleteImportantDay dayId = do
  let key = toSqlKey (fromIntegral dayId) :: Key ImportantDayDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> runDB (delete key) >> return NoContent
