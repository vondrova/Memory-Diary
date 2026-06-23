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

module Lib (startApp, durMins, topN, splitComma, joinComma) where

import Control.Monad (zipWithM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (pack)
import Data.List (group, maximumBy, nub, sort, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, formatTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import System.Environment (lookupEnv)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Multipart
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath (takeExtension, (</>))

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

type AppM = ReaderT ConnectionPool Handler

runDB :: ReaderT SqlBackend IO a -> AppM a
runDB action = ask >>= liftIO . runSqlPool action

type MemoryAPI =
       "api" :> "memories"
         :> QueryParam "q"        Text
         :> QueryParam "tag"      Text
         :> QueryParam "location" Text
         :> QueryParam "from"     Text
         :> QueryParam "to"       Text
         :> Get '[JSON] [Memory]
  :<|> "api" :> "memories" :> ReqBody '[JSON] MemoryInput :> Post '[JSON] Memory
  :<|> "api" :> "memories" :> Capture "id" Int :> ReqBody '[JSON] MemoryInput :> Put '[JSON] Memory
  :<|> "api" :> "memories" :> Capture "id" Int :> Delete '[JSON] NoContent

type PhotoAPI = "api" :> "photos" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] [String]

type FullAPI = MemoryAPI :<|> StatsAPI :<|> PhotoAPI :<|> "photos" :> Raw

memoryServer :: ServerT MemoryAPI AppM
memoryServer =
       getMemories
  :<|> createMemory
  :<|> updateMemory
  :<|> deleteMemory

getMemories :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AppM [Memory]
getMemories mq mTag mLoc mFrom mTo = do
  entities <- runDB $ selectList [] [Desc MemoryDbId]
  return $ filter (matchesFilter mq mTag mLoc mFrom mTo) (map toMemory entities)

matchesFilter :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Memory -> Bool
matchesFilter mq mTag mLoc mFrom mTo mem =
  matchQ mq && matchTag mTag && matchLoc mLoc && matchFrom mFrom && matchTo mTo
  where
    matchQ Nothing  = True
    matchQ (Just q) =
      let ql = T.toLower q
      in T.isInfixOf ql (T.toLower (T.pack (memoryTitle mem)))
         || maybe False (T.isInfixOf ql . T.toLower . T.pack) (memoryDescription mem)

    matchTag Nothing    = True
    matchTag (Just tag) = T.unpack tag `elem` memoryTags mem

    matchLoc Nothing    = True
    matchLoc (Just loc) = maybe False (T.isInfixOf (T.toLower loc) . T.toLower . T.pack) (memoryLocation mem)

    matchFrom Nothing     = True
    matchFrom (Just from) = T.unpack from <= memoryTimeFrom mem

    matchTo Nothing   = True
    matchTo (Just to) = memoryTimeTo mem <= T.unpack to

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

data Stats = Stats
  { statsTotalMinutes    :: Int
  , statsMemoryCount     :: Int
  , statsPhotoCount      :: Int
  , statsVisitedPlaces   :: Int
  , statsAverageMinutes  :: Int
  , statsLongestTitle    :: Maybe String
  , statsLongestMinutes  :: Int
  , statsTopTags         :: [(String, Int)]
  , statsTopLocations    :: [(String, Int)]
  , statsMonthlyActivity :: [(String, Int)]
  , statsDailyActivity   :: [(String, Int)]
  } deriving (Show, Generic)

instance FromJSON Stats
instance ToJSON Stats

type StatsAPI = "api" :> "stats" :> Get '[JSON] Stats

getStats :: AppM Stats
getStats = do
  entities <- runDB $ selectList [] []
  return $ computeStats (map toMemory entities)

computeStats :: [Memory] -> Stats
computeStats mems = Stats
  { statsTotalMinutes    = totalMins
  , statsMemoryCount     = length mems
  , statsPhotoCount      = sum (map (length . memoryPhotos) mems)
  , statsVisitedPlaces   = length (nub (mapMaybe memoryLocation mems))
  , statsAverageMinutes  = if null mems then 0 else totalMins `div` length mems
  , statsLongestTitle    = memoryTitle <$> safeLongest
  , statsLongestMinutes  = maybe 0 (\m -> durMins (memoryTimeFrom m) (memoryTimeTo m)) safeLongest
  , statsTopTags         = topN 10 (concatMap memoryTags mems)
  , statsTopLocations    = topN 10 (mapMaybe memoryLocation mems)
  , statsMonthlyActivity = activityBy (take 7 . memoryTimeFrom) mems
  , statsDailyActivity   = activityBy (take 10 . memoryTimeFrom) mems
  }
  where
    durations   = map (\m -> durMins (memoryTimeFrom m) (memoryTimeTo m)) mems
    totalMins   = sum durations
    safeLongest
      | null mems = Nothing
      | otherwise = Just $ fst $ maximumBy (comparing snd) (zip mems durations)

parseDT :: String -> Maybe UTCTime
parseDT s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" s

durMins :: String -> String -> Int
durMins from to = fromMaybe 0 $ do
  t1 <- parseDT from
  t2 <- parseDT to
  return $ max 0 $ round (diffUTCTime t2 t1 / 60)

topN :: Int -> [String] -> [(String, Int)]
topN n xs =
  take n
  $ sortBy (comparing (Down . snd))
  $ map (\g -> (head g, length g))
  $ group (sort xs)

activityBy :: (Memory -> String) -> [Memory] -> [(String, Int)]
activityBy f mems =
  sortBy (comparing fst)
  $ map (\g -> (head g, length g))
  $ group (sort (map f mems))

photosDir :: FilePath
photosDir = "photos"

uploadPhotos :: MultipartData Tmp -> Handler [String]
uploadPhotos multipartData = liftIO $ do
  createDirectoryIfMissing True photosDir
  now <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y%m%d%H%M%S" now
  zipWithM (saveFile ts) [1 :: Int ..] (files multipartData)
  where
    saveFile ts idx fd = do
      let ext  = takeExtension (T.unpack (fdFileName fd))
          name = ts ++ show idx ++ ext
          dest = photosDir </> name
      copyFile (fdPayload fd) dest
      return name

app :: ConnectionPool -> Application
app pool =
  serve (Proxy :: Proxy FullAPI) $
    hoistServer (Proxy :: Proxy MemoryAPI)   (`runReaderT` pool) memoryServer
    :<|> hoistServer (Proxy :: Proxy StatsAPI) (`runReaderT` pool) getStats
    :<|> uploadPhotos
    :<|> Tagged (staticApp (defaultWebAppSettings photosDir))

startApp :: IO ()
startApp = do
  dbUrl <- fromMaybe "postgresql://memory_diary:memory_diary@localhost:5432/memory_diary" <$> lookupEnv "DATABASE_URL"
  pool  <- runNoLoggingT $ createPostgresqlPool (pack dbUrl) 10
  runSqlPool (runMigration migrateAll) pool
  createDirectoryIfMissing True photosDir
  putStrLn "Memory Diary starting on port 3000..."
  run 3000 (app pool)
