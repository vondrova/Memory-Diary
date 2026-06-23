{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the api memories endpoints
module Handler.Memories
  ( memoryServer,
    listMemories,
    memoryRowFromEntity,
    createMemory,
    getMemory,
    updateMemory,
    deleteMemory,
  )
where

import qualified Api
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Domain.Filter (applyMemoryFilters, parseUtcTime, validFilterRange)
import Domain.Validation (cleanMaybeText, validateMemoryInputFields)
import Handler.Helpers
import Handler.Photos (safeStoredPhotoName)
import Models
import Servant
import Types

memoryServer :: ServerT Api.MemoryApi AppM
memoryServer =
  listMemories
    :<|> createMemory
    :<|> getOnThisDay
    :<|> getMemory
    :<|> updateMemory
    :<|> deleteMemory

-- | Return all memories that match the supplied optional filter parameters
listMemories ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  AppM [MemoryRow]
listMemories mq mtag mloc mfrom mto = do
  if validFilterRange mfrom mto
    then pure ()
    else badRequest "Invalid date filter range: from must not be after to."
  let fromFilter = case mfrom >>= parseUtcTime of
        Nothing -> []
        Just t -> [MemoryDbTimeFrom >=. t]
      toFilter = case mto >>= parseUtcTime of
        Nothing -> []
        Just t -> [MemoryDbTimeTo <=. t]
  rows <- runDb (selectList (fromFilter ++ toFilter) [Asc MemoryDbTimeFrom])
  allRows <- mapM memoryRowFromEntity rows
  pure (applyMemoryFilters mq mtag mloc mfrom mto allRows)

-- | Create a new memory entry and return the persisted row
createMemory :: MemoryInput -> AppM MemoryRow
createMemory input = do
  validateMemory input
  memId <- runDb $ do
    key <- insert (memoryDbFromInput input)
    replaceMemoryTags key (miTags input)
    replaceMemoryPhotos key (miPhotos input)
    ensureMemoryCatalogValues input
    pure key
  getMemory (toIntKey memId)

-- | Retrieve a single memory by its numeric id
getMemory :: Int -> AppM MemoryRow
getMemory rawId = do
  mMem <- runDb (getEntity (fromIntKey rawId :: MemoryDbId))
  maybe notFound memoryRowFromEntity mMem

-- | Replace all fields of an existing memory entry
updateMemory :: Int -> MemoryInput -> AppM NoContent
updateMemory rawId input = do
  validateMemory input
  let memId = fromIntKey rawId :: MemoryDbId
  existing <- runDb (get memId)
  case existing of
    Nothing -> notFound
    Just _ -> do
      runDb $ do
        replace memId (memoryDbFromInput input)
        replaceMemoryTags memId (miTags input)
        replaceMemoryPhotos memId (miPhotos input)
        ensureMemoryCatalogValues input
      pure NoContent

-- | Soft-delete a memory by moving it to the trash table
--   The trash insert and the memory delete run in one transaction
deleteMemory :: Int -> AppM NoContent
deleteMemory rawId = do
  row <- getMemory rawId
  runDb $ do
    now <- liftIO getCurrentTime
    insert_ (TrashItemDb "memory" (encodeToText (memoryInputFromRow row)) now)
    let memId = fromIntKey rawId :: MemoryDbId
    deleteWhere [MemoryTagDbMemoryId ==. memId]
    deleteWhere [MemoryPhotoDbMemoryId ==. memId]
    delete memId
  pure NoContent

-- | Return memories whose day and month match today but from an earlier year
getOnThisDay :: AppM [MemoryRow]
getOnThisDay = do
  now <- liftIO getCurrentTime
  let (thisYear, todayMon, todayDay) = toGregorian (utctDay now)
      matchesDay mem =
        let (year, mon, day) = toGregorian (utctDay (memoryDbTimeFrom mem))
         in mon == todayMon && day == todayDay && year < thisYear
  rows <- runDb (selectList [] [Asc MemoryDbTimeFrom])
  mapM memoryRowFromEntity (filter (matchesDay . entityVal) rows)

-- | Build a 'MemoryRow' by joining a 'MemoryDb' entity with its tags and photos
memoryRowFromEntity :: Entity MemoryDb -> AppM MemoryRow
memoryRowFromEntity (Entity memId mem) = do
  tags <- runDb (selectList [MemoryTagDbMemoryId ==. memId] [Asc MemoryTagDbTag])
  photos <- runDb (selectList [MemoryPhotoDbMemoryId ==. memId] [Asc MemoryPhotoDbPosition])
  pure
    MemoryRow
      { mrId = toIntKey memId,
        mrTitle = memoryDbTitle mem,
        mrTimeFrom = memoryDbTimeFrom mem,
        mrTimeTo = memoryDbTimeTo mem,
        mrDescription = memoryDbDescription mem,
        mrLocation = memoryDbLocation mem,
        mrTags = map (memoryTagDbTag . entityVal) tags,
        mrPhotos = map (memoryPhotoDbFilename . entityVal) photos
      }

-- | Reconstruct a 'MemoryInput' from a row, used when soft-deleting to the trash
memoryInputFromRow :: MemoryRow -> MemoryInput
memoryInputFromRow row =
  MemoryInput
    { miTitle = mrTitle row,
      miTimeFrom = mrTimeFrom row,
      miTimeTo = mrTimeTo row,
      miDescription = mrDescription row,
      miLocation = mrLocation row,
      miTags = mrTags row,
      miPhotos = mrPhotos row
    }

-- | Validate a 'MemoryInput'
validateMemory :: MemoryInput -> AppM ()
validateMemory input = do
  validateInput (validateMemoryInputFields input)
  if all safeStoredPhotoName (miPhotos input)
    then pure ()
    else badRequest "Memory photos must reference stored image filenames only."

-- | Map a 'MemoryInput' to a database record, trimming and normalising text fields
memoryDbFromInput :: MemoryInput -> MemoryDb
memoryDbFromInput input =
  MemoryDb
    { memoryDbTitle = T.strip (miTitle input),
      memoryDbTimeFrom = miTimeFrom input,
      memoryDbTimeTo = miTimeTo input,
      memoryDbDescription = cleanMaybeText (miDescription input),
      memoryDbLocation = cleanMaybeText (miLocation input)
    }

-- | Delete all existing tags for a memory and insert the new list (deduplicated)
replaceMemoryTags :: MemoryDbId -> [Text] -> SqlPersistT IO ()
replaceMemoryTags memId tags = do
  deleteWhere [MemoryTagDbMemoryId ==. memId]
  mapM_ (insert_ . MemoryTagDb memId) (dedupeText tags)

-- | Delete all existing photos for a memory and insert the new list with positions (deduplicated)
replaceMemoryPhotos :: MemoryDbId -> [Text] -> SqlPersistT IO ()
replaceMemoryPhotos memId photos = do
  deleteWhere [MemoryPhotoDbMemoryId ==. memId]
  mapM_ (\(i, fn) -> insert_ (MemoryPhotoDb memId fn i)) (zip [0 ..] (dedupeText photos))

-- | Persist all tag/location options used by a memory independently of that memory
ensureMemoryCatalogValues :: MemoryInput -> SqlPersistT IO ()
ensureMemoryCatalogValues input = do
  mapM_ (insertUnique . TagCatalogDb) (dedupeText (map T.strip (miTags input)))
  case cleanMaybeText (miLocation input) of
    Nothing -> pure ()
    Just location -> do
      _ <- insertUnique (LocationCatalogDb location)
      pure ()
