{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the /api/notes endpoints.
module Handler.Notes
  ( noteServer
  ) where

import Api (NoteAPI)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime, formatTime)
import Data.Time.Format (defaultTimeLocale)
import Handler.Helpers (AppM, runDB)
import Models
import Servant
import Types
import qualified Data.Text as T

noteServer :: ServerT NoteAPI AppM
noteServer = getNotes :<|> createNote :<|> updateNote :<|> deleteNote

-- | Return all notes sorted by creation time descending.
getNotes :: AppM [CoupleNote]
getNotes = map toCoupleNote <$> runDB (selectList [] [Desc CoupleNoteDbCreatedAt])

-- | Create a new note.
createNote :: CoupleNoteInput -> AppM CoupleNote
createNote input = do
  now <- liftIO $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" <$> getCurrentTime
  let db = CoupleNoteDb
        { coupleNoteDbOwner     = T.pack (coupleNoteInputOwner input)
        , coupleNoteDbTitle     = T.pack (coupleNoteInputTitle input)
        , coupleNoteDbBody      = T.pack (coupleNoteInputBody input)
        , coupleNoteDbCreatedAt = T.pack now
        }
  key <- runDB $ insert db
  return (toCoupleNote (Entity key db))

-- | Replace all fields of an existing note.
updateNote :: Int -> CoupleNoteInput -> AppM CoupleNote
updateNote noteId input = do
  let key = toSqlKey (fromIntegral noteId) :: Key CoupleNoteDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just e  -> do
      let db = e
            { coupleNoteDbOwner = T.pack (coupleNoteInputOwner input)
            , coupleNoteDbTitle = T.pack (coupleNoteInputTitle input)
            , coupleNoteDbBody  = T.pack (coupleNoteInputBody input)
            }
      runDB $ replace key db
      return (toCoupleNote (Entity key db))

-- | Delete a note by id.
deleteNote :: Int -> AppM NoContent
deleteNote noteId = do
  let key = toSqlKey (fromIntegral noteId) :: Key CoupleNoteDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> runDB (delete key) >> return NoContent
