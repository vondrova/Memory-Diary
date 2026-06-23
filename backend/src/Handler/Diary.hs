{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the api diary endpoints
--
--   The diary lets each partner keep a personal journal
--   All entries for both partners are returned by a single GET endpoint
--   The Elm frontend filters by owner client-side
--   Every entry date must be in the past or today

module Handler.Diary
  ( diaryServer, 
    listDiaryEntries,
    createDiaryEntry,
    updateDiaryEntry,
    deleteDiaryEntry,
  )
where

import qualified Api
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, getCurrentTime, utctDay)
import Database.Persist
import Handler.Helpers
import Models
import Servant
import Types


-- | Servant server that wires the four diary endpoints to their handlers
diaryServer :: ServerT Api.DiaryApi AppM
diaryServer = listDiaryEntries :<|> createDiaryEntry :<|> updateDiaryEntry :<|> deleteDiaryEntry


-- | Return all diary entries across both partners, sorted newest-first
listDiaryEntries :: AppM [DiaryRow]
listDiaryEntries = do
  entries <- runDb (selectList [] [Desc DiaryEntryDbDate])
  pure (map diaryRowFromEntity entries)


-- | Create a new diary entry and return the persisted row
createDiaryEntry :: DiaryInput -> AppM DiaryRow
createDiaryEntry input = do
  guardNotFuture (deiDate input) -- reject future dates
  entryId <- runDb (insert (diaryDbFromInput input))
  getDiaryEntry (toIntKey entryId)


-- | Replace all fields of an existing diary entry
updateDiaryEntry :: Int -> DiaryInput -> AppM NoContent
updateDiaryEntry rawId input = do
  guardNotFuture (deiDate input) -- reject future dates
  let entryId = fromIntKey rawId :: DiaryEntryDbId
  existing <- runDb (get entryId) -- check that the entry exists before attempting to update
  case existing of
    Nothing -> notFound
    Just _ -> runDb (replace entryId (diaryDbFromInput input)) >> pure NoContent


-- | Permanently delete a diary entry
--   Diary entries are not moved to the trash: journals are personal and accidental deletions are prevented by dialogue window 
deleteDiaryEntry :: Int -> AppM NoContent
deleteDiaryEntry rawId = do
  let entryId = fromIntKey rawId :: DiaryEntryDbId
  existing <- runDb (get entryId)
  case existing of
    Nothing -> notFound
    Just _ -> runDb (delete entryId) >> pure NoContent


-- | Load one diary entry by id and convert it to the API response type 
--   Used internally after insert to return the created row with its generated id
getDiaryEntry :: Int -> AppM DiaryRow
getDiaryEntry rawId = do
  mEntry <- runDb (getEntity (fromIntKey rawId :: DiaryEntryDbId))
  case mEntry of
    Nothing -> notFound
    Just entry -> pure (diaryRowFromEntity entry)


-- | Reject the request if the entry date is in the future
guardNotFuture :: UTCTime -> AppM ()
guardNotFuture date = do
  now <- liftIO getCurrentTime
  when (utctDay date > utctDay now) $
    badRequest "Diary entries cannot be dated in the future."


-- | Convert a 'DiaryInput' request body to a Persistent database record
diaryDbFromInput :: DiaryInput -> DiaryEntryDb
diaryDbFromInput input =
  DiaryEntryDb
    (deiOwner input)
    (deiDate input)
    (deiBody input)


-- | Convert a Persistent 'Entity DiaryEntryDb' to the API response type 'DiaryRow'
diaryRowFromEntity :: Entity DiaryEntryDb -> DiaryRow
diaryRowFromEntity (Entity entryId entry) =
  DiaryRow
    (toIntKey entryId)
    (diaryEntryDbOwner entry)
    (diaryEntryDbDate entry)
    (diaryEntryDbBody entry)
