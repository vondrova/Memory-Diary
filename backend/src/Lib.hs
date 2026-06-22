{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib (startApp) where

import Data.Aeson (FromJSON, ToJSON)
import Data.IORef
import Data.List (find)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

-- | A single memory entry stored in the diary
data Memory = Memory
  { memoryId          :: Int
  , memoryTitle       :: String
  , memoryTimeFrom    :: String
  , memoryTimeTo      :: String
  , memoryDescription :: Maybe String
  , memoryLocation    :: Maybe String
  , memoryTags        :: [String]
  , memoryPhotos      :: [String]
  } deriving (Eq, Show, Generic)

instance FromJSON Memory
instance ToJSON Memory

-- | Fields sent by the client when creating or updating a memory (no id)
data MemoryInput = MemoryInput
  { inputTitle       :: String
  , inputTimeFrom    :: String
  , inputTimeTo      :: String
  , inputDescription :: Maybe String
  , inputLocation    :: Maybe String
  , inputTags        :: [String]
  , inputPhotos      :: [String]
  } deriving (Eq, Show, Generic)

instance FromJSON MemoryInput
instance ToJSON MemoryInput

-- | Servant API definition for the memories resource
type MemoryAPI =
       "api" :> "memories" :> Get '[JSON] [Memory]
  :<|> "api" :> "memories" :> ReqBody '[JSON] MemoryInput :> Post '[JSON] Memory
  :<|> "api" :> "memories" :> Capture "id" Int :> ReqBody '[JSON] MemoryInput :> Put '[JSON] Memory
  :<|> "api" :> "memories" :> Capture "id" Int :> Delete '[JSON] NoContent

memoryServer :: IORef [Memory] -> IORef Int -> Server MemoryAPI
memoryServer ref nextId =
       getMemories ref
  :<|> createMemory ref nextId
  :<|> updateMemory ref
  :<|> deleteMemory ref

getMemories :: IORef [Memory] -> Handler [Memory]
getMemories ref = liftIO $ reverse <$> readIORef ref

createMemory :: IORef [Memory] -> IORef Int -> MemoryInput -> Handler Memory
createMemory ref nextId input = liftIO $ do
  newId <- readIORef nextId
  writeIORef nextId (newId + 1)
  let memory = Memory
        { memoryId          = newId
        , memoryTitle       = inputTitle input
        , memoryTimeFrom    = inputTimeFrom input
        , memoryTimeTo      = inputTimeTo input
        , memoryDescription = inputDescription input
        , memoryLocation    = inputLocation input
        , memoryTags        = inputTags input
        , memoryPhotos      = inputPhotos input
        }
  modifyIORef ref (memory :)
  return memory

updateMemory :: IORef [Memory] -> Int -> MemoryInput -> Handler Memory
updateMemory ref memId input = do
  memories <- liftIO $ readIORef ref
  case find (\m -> memoryId m == memId) memories of
    Nothing -> throwError err404
    Just _  -> do
      let updated = Memory
            { memoryId          = memId
            , memoryTitle       = inputTitle input
            , memoryTimeFrom    = inputTimeFrom input
            , memoryTimeTo      = inputTimeTo input
            , memoryDescription = inputDescription input
            , memoryLocation    = inputLocation input
            , memoryTags        = inputTags input
            , memoryPhotos      = inputPhotos input
            }
      liftIO $ modifyIORef ref (map (\m -> if memoryId m == memId then updated else m))
      return updated

deleteMemory :: IORef [Memory] -> Int -> Handler NoContent
deleteMemory ref memId = do
  memories <- liftIO $ readIORef ref
  case find (\m -> memoryId m == memId) memories of
    Nothing -> throwError err404
    Just _  -> do
      liftIO $ modifyIORef ref (filter (\m -> memoryId m /= memId))
      return NoContent

app :: IORef [Memory] -> IORef Int -> Application
app ref nextId = serve (Proxy :: Proxy MemoryAPI) (memoryServer ref nextId)

startApp :: IO ()
startApp = do
  ref    <- newIORef []
  nextId <- newIORef 1
  putStrLn "Memory Diary starting on port 3000..."
  run 3000 (app ref nextId)
