{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the api trash endpoints, allowing soft-deleted items to be listed, restored or permanently deleted
--
-- The trash table is a short-lived soft-delete buffer - items older than 24 hour are purged automatically on every list request
module Handler.Trash
  ( trashServer,
    listTrash,
    restoreTrashItem,
    deleteTrashItemPermanent,
  )
where

import qualified Api
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Database.Persist
import Handler.Helpers
import Handler.ImportantDays (createImportantDay)
import Handler.Memories (createMemory)
import Handler.Notes (createNote)
import Handler.Plans (createPlan)
import Models
import Servant
import Types

trashServer :: ServerT Api.TrashApi AppM
trashServer = listTrash :<|> restoreTrashItem :<|> deleteTrashItemPermanent

-- | Return all trash items, purging entries older than 24 hours as a side effect
listTrash :: AppM [TrashRow]
listTrash = do
  now <- liftIO getCurrentTime
  runDb (deleteWhere [TrashItemDbDeletedAt <. addUTCTime (negate 86400) now])
  items <- runDb (selectList [] [Desc TrashItemDbDeletedAt])
  pure (map trashRowFromEntity items)

-- | Restore a trashed item by decoding its payload and re-creating it, then deleting the trash entry
restoreTrashItem rawId = do
  let trashId = fromIntKey rawId :: TrashItemDbId
  mItem <- runDb (get trashId)
  case mItem of
    Nothing -> notFound
    Just item -> do
      dispatchRestore (trashItemDbKind item) (trashItemDbPayload item)
      runDb (delete trashId)
      pure NoContent

-- | Permanently delete a trash item without restoring it
deleteTrashItemPermanent :: Int -> AppM NoContent
deleteTrashItemPermanent rawId =
  runDb (delete (fromIntKey rawId :: TrashItemDbId)) >> pure NoContent

-- | Dispatch a restore operation to the correct handler based on the item kind
dispatchRestore :: Text -> Text -> AppM ()
dispatchRestore kind payload =
  case kind of
    "memory" -> decodeAndRestore createMemory payload
    "important-day" -> decodeAndRestore createImportantDay payload
    "note" -> decodeAndRestore createNote payload
    "plan" -> decodeAndRestore createPlan payload
    _ -> badRequest "Unknown trash item kind."

-- | Decode a JSON payload and pass the value to the given create handler
decodeAndRestore :: (A.FromJSON a) => (a -> AppM b) -> Text -> AppM ()
decodeAndRestore create payload =
  case A.eitherDecodeStrict (TE.encodeUtf8 payload) of
    Left _ -> badRequest "Cannot decode trash item payload."
    Right value -> void (create value)

-- | Convert a Persistent entity to the JSON row type
trashRowFromEntity :: Entity TrashItemDb -> TrashRow
trashRowFromEntity (Entity trashId item) =
  TrashRow
    { trId = toIntKey trashId,
      trKind = trashItemDbKind item,
      trName = extractName (trashItemDbPayload item),
      trDeletedAt = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (trashItemDbDeletedAt item))
    }
