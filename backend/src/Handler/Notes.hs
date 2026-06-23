{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the api notes endpoints
module Handler.Notes
  ( noteServer,
    listNotes,
    createNote,
    getNote,
    updateNote,
    deleteNote,
  )
where

import qualified Api
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist
import Domain.Validation (validateNoteInputFields)
import Handler.Helpers
import Models
import Servant
import Types

noteServer :: ServerT Api.NoteApi AppM
noteServer = listNotes :<|> createNote :<|> updateNote :<|> deleteNote

-- | Return all notes sorted by stable owner key then title
listNotes :: AppM [NoteRow]
listNotes = do
  notes <- runDb (selectList [] [Asc CoupleNoteDbOwner, Asc CoupleNoteDbTitle])
  pure (map noteRowFromEntity notes)

-- | Create a new note and return the persisted row
createNote :: NoteInput -> AppM NoteRow
createNote input = do
  validateNote input
  ensureUniqueNoteTitle Nothing input
  noteId <- runDb (insert (noteDbFromInput input))
  getNote (toIntKey noteId)

-- | Replace all fields of an existing note
updateNote :: Int -> NoteInput -> AppM NoContent
updateNote rawId input = do
  validateNote input
  let noteId = fromIntKey rawId :: CoupleNoteDbId
  existing <- runDb (get noteId)
  case existing of
    Nothing -> notFound
    Just _ -> do
      ensureUniqueNoteTitle (Just noteId) input
      runDb (replace noteId (noteDbFromInput input)) >> pure NoContent

-- | Soft-delete a note by moving it to the trash table
--   The trash insert and the row delete run in one transaction
deleteNote :: Int -> AppM NoContent
deleteNote rawId = do
  row <- getNote rawId
  runDb $ do
    now <- liftIO getCurrentTime
    insert_ (TrashItemDb "note" (encodeToText (NoteInput (nrOwner row) (nrTitle row) (nrBody row))) now)
    delete (fromIntKey rawId :: CoupleNoteDbId)
  pure NoContent

-- | Fetch a single note by numeric id
getNote :: Int -> AppM NoteRow
getNote rawId = do
  mNote <- runDb (getEntity (fromIntKey rawId :: CoupleNoteDbId))
  case mNote of
    Nothing -> notFound
    Just note -> pure (noteRowFromEntity note)

-- | Validate a 'NoteInput'
validateNote :: NoteInput -> AppM ()
validateNote = validateInput . validateNoteInputFields

-- | Ensure that the DB-level @(owner, title) uniqueness constraint is reported as a validation error instead of surfacing as a database exception
ensureUniqueNoteTitle :: Maybe CoupleNoteDbId -> NoteInput -> AppM ()
ensureUniqueNoteTitle currentId input = do
  existing <- runDb (getBy (UniqueNoteTitle owner title))
  case existing of
    Nothing ->
      pure ()
    Just (Entity foundId _) ->
      if Just foundId == currentId
        then pure ()
        else badRequest "A note with this owner and title already exists."
  where
    owner = T.toLower (T.strip (niOwner input))
    title = T.strip (niTitle input)

-- | Map a 'NoteInput' to a database record, trimming all text fields
noteDbFromInput :: NoteInput -> CoupleNoteDb
noteDbFromInput input =
  CoupleNoteDb
    (T.toLower (T.strip (niOwner input)))
    (T.strip (niTitle input))
    (T.strip (niBody input))

-- | Convert a Persistent entity to the JSON row type
noteRowFromEntity :: Entity CoupleNoteDb -> NoteRow
noteRowFromEntity (Entity noteId note) =
  NoteRow
    (toIntKey noteId)
    (coupleNoteDbOwner note)
    (coupleNoteDbTitle note)
    (coupleNoteDbBody note)
