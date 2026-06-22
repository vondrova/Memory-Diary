{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib (startApp) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

-- | Database schema — tags and photos stored as comma-separated text for now
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MemoryDb
  title       Text
  timeFrom    Text
  timeTo      Text
  description Text Maybe
  location    Text Maybe
  tags        Text
  photos      Text
  deriving Show
|]

-- | JSON response type for the API
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

-- | Request body for creating or updating a memory
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

-- | Convert a DB row to the API response type
toMemory :: Entity MemoryDb -> Memory
toMemory (Entity key db) = Memory
  { memoryId          = fromIntegral (fromSqlKey key)
  , memoryTitle       = T.unpack (memoryDbTitle db)
  , memoryTimeFrom    = T.unpack (memoryDbTimeFrom db)
  , memoryTimeTo      = T.unpack (memoryDbTimeTo db)
  , memoryDescription = T.unpack <$> memoryDbDescription db
  , memoryLocation    = T.unpack <$> memoryDbLocation db
  , memoryTags        = splitComma (memoryDbTags db)
  , memoryPhotos      = splitComma (memoryDbPhotos db)
  }

splitComma :: Text -> [String]
splitComma t
  | T.null t  = []
  | otherwise = map T.unpack (T.splitOn "," t)

joinComma :: [String] -> Text
joinComma = T.intercalate "," . map T.pack

-- | Application monad: Handler extended with a DB connection pool
type AppM = ReaderT ConnectionPool Handler

-- | Run a database action inside AppM
runDB :: ReaderT SqlBackend IO a -> AppM a
runDB action = ask >>= liftIO . runSqlPool action

-- | Servant API type
type MemoryAPI =
       "api" :> "memories" :> Get '[JSON] [Memory]
  :<|> "api" :> "memories" :> ReqBody '[JSON] MemoryInput :> Post '[JSON] Memory
  :<|> "api" :> "memories" :> Capture "id" Int :> ReqBody '[JSON] MemoryInput :> Put '[JSON] Memory
  :<|> "api" :> "memories" :> Capture "id" Int :> Delete '[JSON] NoContent

memoryServer :: ServerT MemoryAPI AppM
memoryServer =
       getMemories
  :<|> createMemory
  :<|> updateMemory
  :<|> deleteMemory

getMemories :: AppM [Memory]
getMemories = do
  entities <- runDB $ selectList [] [Desc MemoryDbId]
  return (map toMemory entities)

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

deleteMemory :: Int -> AppM NoContent
deleteMemory memId = do
  let key = toSqlKey (fromIntegral memId) :: Key MemoryDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> do
      runDB $ delete key
      return NoContent

app :: ConnectionPool -> Application
app pool =
  serve (Proxy :: Proxy MemoryAPI) $
    hoistServer (Proxy :: Proxy MemoryAPI) (`runReaderT` pool) memoryServer

startApp :: IO ()
startApp = do
  pool <- runNoLoggingT $ createSqlitePool "memory-diary.db" 5
  runSqlPool (runMigration migrateAll) pool
  putStrLn "Memory Diary starting on port 3000..."
  run 3000 (app pool)
