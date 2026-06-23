{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the /api/diary endpoints.
module Handler.Diary
  ( diaryServer
  ) where

import Api (DiaryAPI)
import Handler.Helpers (AppM, runDB)
import Models
import Servant
import Types
import qualified Data.Text as T

diaryServer :: ServerT DiaryAPI AppM
diaryServer = getDiaryEntries :<|> createDiaryEntry :<|> updateDiaryEntry :<|> deleteDiaryEntry

-- | Return all diary entries sorted newest-first.
getDiaryEntries :: AppM [DiaryEntry]
getDiaryEntries = map toDiaryEntry <$> runDB (selectList [] [Desc DiaryEntryDbDate])

-- | Create a new diary entry.
createDiaryEntry :: DiaryEntryInput -> AppM DiaryEntry
createDiaryEntry input = do
  let db = DiaryEntryDb
        { diaryEntryDbDate    = T.pack (diaryEntryInputDate input)
        , diaryEntryDbMood    = T.pack <$> diaryEntryInputMood input
        , diaryEntryDbBody    = T.pack (diaryEntryInputBody input)
        , diaryEntryDbWeather = T.pack <$> diaryEntryInputWeather input
        }
  key <- runDB $ insert db
  return (toDiaryEntry (Entity key db))

-- | Replace all fields of an existing diary entry.
updateDiaryEntry :: Int -> DiaryEntryInput -> AppM DiaryEntry
updateDiaryEntry entryId input = do
  let key = toSqlKey (fromIntegral entryId) :: Key DiaryEntryDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> do
      let db = DiaryEntryDb
            { diaryEntryDbDate    = T.pack (diaryEntryInputDate input)
            , diaryEntryDbMood    = T.pack <$> diaryEntryInputMood input
            , diaryEntryDbBody    = T.pack (diaryEntryInputBody input)
            , diaryEntryDbWeather = T.pack <$> diaryEntryInputWeather input
            }
      runDB $ replace key db
      return (toDiaryEntry (Entity key db))

-- | Permanently delete a diary entry by id.
deleteDiaryEntry :: Int -> AppM NoContent
deleteDiaryEntry entryId = do
  let key = toSqlKey (fromIntegral entryId) :: Key DiaryEntryDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> runDB (delete key) >> return NoContent
