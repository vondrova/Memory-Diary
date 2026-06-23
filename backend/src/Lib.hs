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

module Lib (startApp, durMins, topN, splitComma, joinComma, computeNextOccurrence, parseDay) where

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
import Data.Time (UTCTime, diffUTCTime, formatTime, getCurrentTime, utctDay)
import Data.Time.Calendar (Day, diffDays, fromGregorian, toGregorian)
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

ImportantDayDb
  title Text
  date  Text
  note  Text Maybe
  kind  Text
  deriving Show

RelationshipDb
  partner1  Text
  partner2  Text
  startDate Text
  note      Text Maybe
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

type FullAPI = MemoryAPI :<|> ImportantDayAPI :<|> RelationshipAPI :<|> StatsAPI :<|> PhotoAPI :<|> "photos" :> Raw

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

data ImportantDay = ImportantDay
  { importantDayId             :: Maybe Int
  , importantDayTitle          :: String
  , importantDayDate           :: String
  , importantDayNote           :: Maybe String
  , importantDayKind           :: String
  , importantDayNextOccurrence :: String
  , importantDayDaysUntil      :: Int
  , importantDayMonth          :: Int
  , importantDayDay            :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON ImportantDay
instance ToJSON ImportantDay

data ImportantDayInput = ImportantDayInput
  { importantDayInputTitle :: String
  , importantDayInputDate  :: String
  , importantDayInputNote  :: Maybe String
  , importantDayInputKind  :: String
  } deriving (Eq, Show, Generic)

instance FromJSON ImportantDayInput
instance ToJSON ImportantDayInput

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"

computeNextOccurrence :: Day -> String -> (String, Int, Int, Int)
computeNextOccurrence today dateStr =
  case parseDay dateStr of
    Nothing -> (dateStr, 0, 0, 0)
    Just d  ->
      let (_, m, dd)    = toGregorian d
          (yr, _, _)    = toGregorian today
          thisYearDate  = fromGregorian yr m dd
          nextDate      = if today <= thisYearDate then thisYearDate
                          else fromGregorian (yr + 1) m dd
          days          = fromIntegral (diffDays nextDate today)
      in (formatDay nextDate, days, fromIntegral m, fromIntegral dd)

toImportantDay :: Day -> Entity ImportantDayDb -> ImportantDay
toImportantDay today (Entity key db) =
  let (nextOcc, days, month, day) = computeNextOccurrence today (T.unpack (importantDayDbDate db))
  in ImportantDay
    { importantDayId             = Just (fromIntegral (fromSqlKey key))
    , importantDayTitle          = T.unpack (importantDayDbTitle db)
    , importantDayDate           = T.unpack (importantDayDbDate db)
    , importantDayNote           = T.unpack <$> importantDayDbNote db
    , importantDayKind           = T.unpack (importantDayDbKind db)
    , importantDayNextOccurrence = nextOcc
    , importantDayDaysUntil      = days
    , importantDayMonth          = month
    , importantDayDay            = day
    }

type ImportantDayAPI =
       "api" :> "important-days" :> Get '[JSON] [ImportantDay]
  :<|> "api" :> "important-days" :> ReqBody '[JSON] ImportantDayInput :> Post '[JSON] ImportantDay
  :<|> "api" :> "important-days" :> Capture "id" Int :> ReqBody '[JSON] ImportantDayInput :> Put '[JSON] ImportantDay
  :<|> "api" :> "important-days" :> Capture "id" Int :> Delete '[JSON] NoContent

importantDayServer :: ServerT ImportantDayAPI AppM
importantDayServer =
       getImportantDays
  :<|> createImportantDay
  :<|> updateImportantDay
  :<|> deleteImportantDay

getImportantDays :: AppM [ImportantDay]
getImportantDays = do
  today    <- liftIO $ utctDay <$> getCurrentTime
  entities <- runDB $ selectList [] [Asc ImportantDayDbDate]
  return $ map (toImportantDay today) entities

createImportantDay :: ImportantDayInput -> AppM ImportantDay
createImportantDay input = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let db = ImportantDayDb
        { importantDayDbTitle = T.pack (importantDayInputTitle input)
        , importantDayDbDate  = T.pack (importantDayInputDate input)
        , importantDayDbNote  = T.pack <$> importantDayInputNote input
        , importantDayDbKind  = T.pack (importantDayInputKind input)
        }
  key <- runDB $ insert db
  return (toImportantDay today (Entity key db))

updateImportantDay :: Int -> ImportantDayInput -> AppM ImportantDay
updateImportantDay dayId input = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let key = toSqlKey (fromIntegral dayId) :: Key ImportantDayDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> do
      let db = ImportantDayDb
            { importantDayDbTitle = T.pack (importantDayInputTitle input)
            , importantDayDbDate  = T.pack (importantDayInputDate input)
            , importantDayDbNote  = T.pack <$> importantDayInputNote input
            , importantDayDbKind  = T.pack (importantDayInputKind input)
            }
      runDB $ replace key db
      return (toImportantDay today (Entity key db))

deleteImportantDay :: Int -> AppM NoContent
deleteImportantDay dayId = do
  let key = toSqlKey (fromIntegral dayId) :: Key ImportantDayDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> do
      runDB $ delete key
      return NoContent

data Relationship = Relationship
  { relationshipId              :: Maybe Int
  , relationshipPartner1        :: String
  , relationshipPartner2        :: String
  , relationshipStartDate       :: String
  , relationshipNote            :: Maybe String
  , relationshipDaysTogether    :: Int
  , relationshipYearsTogether   :: Int
  , relationshipNextAnniversary :: String
  , relationshipDaysUntilAnniv  :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Relationship
instance ToJSON Relationship

data RelationshipInput = RelationshipInput
  { relationshipInputPartner1  :: String
  , relationshipInputPartner2  :: String
  , relationshipInputStartDate :: String
  , relationshipInputNote      :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromJSON RelationshipInput
instance ToJSON RelationshipInput

toRelationship :: Day -> Entity RelationshipDb -> Relationship
toRelationship today (Entity key db) =
  let startStr = T.unpack (relationshipDbStartDate db)
      daysTog  = maybe 0 (fromIntegral . diffDays today) (parseDay startStr)
      yearsTog = daysTog `div` 365
      (nextAnn, daysUntilAnn, _, _) = computeNextOccurrence today startStr
  in Relationship
    { relationshipId              = Just (fromIntegral (fromSqlKey key))
    , relationshipPartner1        = T.unpack (relationshipDbPartner1 db)
    , relationshipPartner2        = T.unpack (relationshipDbPartner2 db)
    , relationshipStartDate       = startStr
    , relationshipNote            = T.unpack <$> relationshipDbNote db
    , relationshipDaysTogether    = daysTog
    , relationshipYearsTogether   = yearsTog
    , relationshipNextAnniversary = nextAnn
    , relationshipDaysUntilAnniv  = daysUntilAnn
    }

type RelationshipAPI =
       "api" :> "relationship" :> Get '[JSON] (Maybe Relationship)
  :<|> "api" :> "relationship" :> ReqBody '[JSON] RelationshipInput :> Put '[JSON] Relationship

relationshipServer :: ServerT RelationshipAPI AppM
relationshipServer = getRelationship :<|> upsertRelationship

getRelationship :: AppM (Maybe Relationship)
getRelationship = do
  today    <- liftIO $ utctDay <$> getCurrentTime
  entities <- runDB $ selectList [] [LimitTo 1]
  return $ case entities of
    []      -> Nothing
    (e : _) -> Just (toRelationship today e)

upsertRelationship :: RelationshipInput -> AppM Relationship
upsertRelationship input = do
  today    <- liftIO $ utctDay <$> getCurrentTime
  entities <- runDB $ selectList [] [LimitTo 1]
  let db = RelationshipDb
        { relationshipDbPartner1  = T.pack (relationshipInputPartner1 input)
        , relationshipDbPartner2  = T.pack (relationshipInputPartner2 input)
        , relationshipDbStartDate = T.pack (relationshipInputStartDate input)
        , relationshipDbNote      = T.pack <$> relationshipInputNote input
        }
  key <- case entities of
    []              -> runDB $ insert db
    (Entity k _ : _) -> runDB (replace k db) >> return k
  return (toRelationship today (Entity key db))

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
    hoistServer (Proxy :: Proxy MemoryAPI)          (`runReaderT` pool) memoryServer
    :<|> hoistServer (Proxy :: Proxy ImportantDayAPI)  (`runReaderT` pool) importantDayServer
    :<|> hoistServer (Proxy :: Proxy RelationshipAPI)  (`runReaderT` pool) relationshipServer
    :<|> hoistServer (Proxy :: Proxy StatsAPI)      (`runReaderT` pool) getStats
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
