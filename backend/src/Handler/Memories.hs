{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the /api/memories endpoints.
module Handler.Memories
  ( memoryServer
  ) where

import Api (MemoryAPI)
import Data.Text (Text)
import Domain.Filter (applyMemoryFilters)
import Handler.Helpers (AppM, runDB)
import Models
import Servant
import Types
import qualified Data.Text as T

memoryServer :: ServerT MemoryAPI AppM
memoryServer = getMemories :<|> createMemory :<|> updateMemory :<|> deleteMemory

-- | Return all memories that match the supplied optional filter parameters.
getMemories :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AppM [Memory]
getMemories mq mTag mLoc mFrom mTo = do
  entities <- runDB $ selectList [] []
  return $ applyMemoryFilters mq mTag mLoc mFrom mTo (map toMemory entities)

-- | Create a new memory entry and return the persisted row.
createMemory :: MemoryInput -> AppM Memory
createMemory input = do
  let db = MemoryDb
        { memoryDbTitle       = T.pack (inputTitle input)
        , memoryDbTimeFrom    = T.pack (inputTimeFrom input)
        , memoryDbTimeTo      = T.pack (inputTimeTo input)
        , memoryDbDescription = T.pack <$> inputDescription input
        , memoryDbLocation    = T.pack <$> inputLocation input
        , memoryDbTags        = joinComma (inputTags input)
        , memoryDbPhotos      = joinComma (inputPhotos input)
        }
  key <- runDB $ insert db
  return (toMemory (Entity key db))

-- | Replace all fields of an existing memory entry.
updateMemory :: Int -> MemoryInput -> AppM Memory
updateMemory memId input = do
  let key = toSqlKey (fromIntegral memId) :: Key MemoryDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> do
      let db = MemoryDb
            { memoryDbTitle       = T.pack (inputTitle input)
            , memoryDbTimeFrom    = T.pack (inputTimeFrom input)
            , memoryDbTimeTo      = T.pack (inputTimeTo input)
            , memoryDbDescription = T.pack <$> inputDescription input
            , memoryDbLocation    = T.pack <$> inputLocation input
            , memoryDbTags        = joinComma (inputTags input)
            , memoryDbPhotos      = joinComma (inputPhotos input)
            }
      runDB $ replace key db
      return (toMemory (Entity key db))

-- | Delete a memory by id.
deleteMemory :: Int -> AppM NoContent
deleteMemory memId = do
  let key = toSqlKey (fromIntegral memId) :: Key MemoryDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> runDB (delete key) >> return NoContent
