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

module Lib (startApp, Memory(..), durMins, topN, splitComma, joinComma, computeNextOccurrence, parseDay, monthDay, computeStats, Stats(..)) where

import Control.Monad (zipWithM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON (..), ToJSON (..), Options, defaultOptions, decode, genericToJSON, genericParseJSON, object, withObject, (.=), (.:))
import Data.Aeson (fieldLabelModifier)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.List (group, isPrefixOf, maximumBy, nub, sort, sortBy, stripPrefix)
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
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest, setRequestHeader)
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

CoupleNoteDb
  owner     Text
  title     Text
  body      Text
  createdAt Text
  deriving Show

CouplePlanDb
  category Text
  title    Text
  detail   Text Maybe
  done     Bool
  deriving Show

DiaryEntryDb
  date    Text
  mood    Text Maybe
  body    Text
  weather Text Maybe
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

instance FromJSON Memory where parseJSON = genericParseJSON (aesonOpts "memory")
instance ToJSON Memory where toJSON = genericToJSON (aesonOpts "memory")

data MemoryInput = MemoryInput
  { inputTitle       :: String
  , inputTimeFrom    :: String
  , inputTimeTo      :: String
  , inputDescription :: Maybe String
  , inputLocation    :: Maybe String
  , inputTags        :: [String]
  , inputPhotos      :: [String]
  } deriving (Eq, Show, Generic)

instance FromJSON MemoryInput where parseJSON = genericParseJSON (aesonOpts "input")
instance ToJSON MemoryInput where toJSON = genericToJSON (aesonOpts "input")

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

aesonOpts :: String -> Options
aesonOpts prefix = defaultOptions
  { fieldLabelModifier = \s -> case stripPrefix prefix s of
      Just (c : cs) -> toLower c : cs
      _             -> s
  }

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

type FullAPI =
       MemoryAPI
  :<|> ImportantDayAPI
  :<|> RelationshipAPI
  :<|> OnThisDayAPI
  :<|> NoteAPI
  :<|> PlanAPI
  :<|> DiaryAPI
  :<|> StatsAPI
  :<|> GeoAPI
  :<|> PhotoAPI
  :<|> "photos" :> Raw

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

instance FromJSON ImportantDay where parseJSON = genericParseJSON (aesonOpts "importantDay")
instance ToJSON ImportantDay where toJSON = genericToJSON (aesonOpts "importantDay")

data ImportantDayInput = ImportantDayInput
  { importantDayInputTitle :: String
  , importantDayInputDate  :: String
  , importantDayInputNote  :: Maybe String
  , importantDayInputKind  :: String
  } deriving (Eq, Show, Generic)

instance FromJSON ImportantDayInput where parseJSON = genericParseJSON (aesonOpts "importantDayInput")
instance ToJSON ImportantDayInput where toJSON = genericToJSON (aesonOpts "importantDayInput")

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

instance FromJSON Relationship where parseJSON = genericParseJSON (aesonOpts "relationship")
instance ToJSON Relationship where toJSON = genericToJSON (aesonOpts "relationship")

data RelationshipInput = RelationshipInput
  { relationshipInputPartner1  :: String
  , relationshipInputPartner2  :: String
  , relationshipInputStartDate :: String
  , relationshipInputNote      :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromJSON RelationshipInput where parseJSON = genericParseJSON (aesonOpts "relationshipInput")
instance ToJSON RelationshipInput where toJSON = genericToJSON (aesonOpts "relationshipInput")

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

monthDay :: String -> String
monthDay s = take 5 (drop 5 s)

data OnThisDay = OnThisDay
  { onThisDayMemories      :: [Memory]
  , onThisDayImportantDays :: [ImportantDay]
  , onThisDayIsAnniversary :: Bool
  , onThisDayYears         :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON OnThisDay where parseJSON = genericParseJSON (aesonOpts "onThisDay")
instance ToJSON OnThisDay where toJSON = genericToJSON (aesonOpts "onThisDay")

type OnThisDayAPI = "api" :> "on-this-day" :> Get '[JSON] OnThisDay

getOnThisDay :: AppM OnThisDay
getOnThisDay = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let todayMD = formatTime defaultTimeLocale "%m-%d" today

  memEntities <- runDB $ selectList [] []
  let todayMems = filter (\m -> monthDay (memoryTimeFrom m) == todayMD)
                         (map toMemory memEntities)

  dayEntities <- runDB $ selectList [] []
  let todayDays = filter (\d -> monthDay (importantDayDate d) == todayMD)
                         (map (toImportantDay today) dayEntities)

  relEntities <- runDB $ (selectList [] [LimitTo 1] :: ReaderT SqlBackend IO [Entity RelationshipDb])
  let mRel    = case relEntities of
                  []      -> Nothing
                  (e : _) -> Just (toRelationship today e)
  let isAnniv = maybe False (\r -> monthDay (relationshipStartDate r) == todayMD
                                && relationshipStartDate r /= formatDay today) mRel
  let years   = if isAnniv then fmap relationshipYearsTogether mRel else Nothing

  return $ OnThisDay
    { onThisDayMemories      = todayMems
    , onThisDayImportantDays = todayDays
    , onThisDayIsAnniversary = isAnniv
    , onThisDayYears         = years
    }

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

instance FromJSON Stats where parseJSON = genericParseJSON (aesonOpts "stats")
instance ToJSON Stats where toJSON = genericToJSON (aesonOpts "stats")

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

-- COUPLE NOTES

data CoupleNote = CoupleNote
  { coupleNoteId        :: Maybe Int
  , coupleNoteOwner     :: String
  , coupleNoteTitle     :: String
  , coupleNoteBody      :: String
  , coupleNoteCreatedAt :: String
  } deriving (Eq, Show, Generic)

instance FromJSON CoupleNote where parseJSON = genericParseJSON (aesonOpts "coupleNote")
instance ToJSON CoupleNote where toJSON = genericToJSON (aesonOpts "coupleNote")

data CoupleNoteInput = CoupleNoteInput
  { coupleNoteInputOwner :: String
  , coupleNoteInputTitle :: String
  , coupleNoteInputBody  :: String
  } deriving (Eq, Show, Generic)

instance FromJSON CoupleNoteInput where parseJSON = genericParseJSON (aesonOpts "coupleNoteInput")
instance ToJSON CoupleNoteInput where toJSON = genericToJSON (aesonOpts "coupleNoteInput")

toCoupleNote :: Entity CoupleNoteDb -> CoupleNote
toCoupleNote (Entity key db) = CoupleNote
  { coupleNoteId        = Just (fromIntegral (fromSqlKey key))
  , coupleNoteOwner     = T.unpack (coupleNoteDbOwner db)
  , coupleNoteTitle     = T.unpack (coupleNoteDbTitle db)
  , coupleNoteBody      = T.unpack (coupleNoteDbBody db)
  , coupleNoteCreatedAt = T.unpack (coupleNoteDbCreatedAt db)
  }

type NoteAPI =
       "api" :> "notes" :> Get '[JSON] [CoupleNote]
  :<|> "api" :> "notes" :> ReqBody '[JSON] CoupleNoteInput :> Post '[JSON] CoupleNote
  :<|> "api" :> "notes" :> Capture "id" Int :> ReqBody '[JSON] CoupleNoteInput :> Put '[JSON] CoupleNote
  :<|> "api" :> "notes" :> Capture "id" Int :> Delete '[JSON] NoContent

noteServer :: ServerT NoteAPI AppM
noteServer = getNotes :<|> createNote :<|> updateNote :<|> deleteNote

getNotes :: AppM [CoupleNote]
getNotes = map toCoupleNote <$> runDB (selectList [] [Desc CoupleNoteDbCreatedAt])

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

deleteNote :: Int -> AppM NoContent
deleteNote noteId = do
  let key = toSqlKey (fromIntegral noteId) :: Key CoupleNoteDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> runDB (delete key) >> return NoContent

-- COUPLE PLANS

data CouplePlan = CouplePlan
  { couplePlanId       :: Maybe Int
  , couplePlanCategory :: String
  , couplePlanTitle    :: String
  , couplePlanDetail   :: Maybe String
  , couplePlanDone     :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON CouplePlan where parseJSON = genericParseJSON (aesonOpts "couplePlan")
instance ToJSON CouplePlan where toJSON = genericToJSON (aesonOpts "couplePlan")

data CouplePlanInput = CouplePlanInput
  { couplePlanInputCategory :: String
  , couplePlanInputTitle    :: String
  , couplePlanInputDetail   :: Maybe String
  , couplePlanInputDone     :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON CouplePlanInput where parseJSON = genericParseJSON (aesonOpts "couplePlanInput")
instance ToJSON CouplePlanInput where toJSON = genericToJSON (aesonOpts "couplePlanInput")

toCouplePlan :: Entity CouplePlanDb -> CouplePlan
toCouplePlan (Entity key db) = CouplePlan
  { couplePlanId       = Just (fromIntegral (fromSqlKey key))
  , couplePlanCategory = T.unpack (couplePlanDbCategory db)
  , couplePlanTitle    = T.unpack (couplePlanDbTitle db)
  , couplePlanDetail   = T.unpack <$> couplePlanDbDetail db
  , couplePlanDone     = couplePlanDbDone db
  }

type PlanAPI =
       "api" :> "plans" :> Get '[JSON] [CouplePlan]
  :<|> "api" :> "plans" :> ReqBody '[JSON] CouplePlanInput :> Post '[JSON] CouplePlan
  :<|> "api" :> "plans" :> Capture "id" Int :> ReqBody '[JSON] CouplePlanInput :> Put '[JSON] CouplePlan
  :<|> "api" :> "plans" :> Capture "id" Int :> Delete '[JSON] NoContent

planServer :: ServerT PlanAPI AppM
planServer = getPlans :<|> createPlan :<|> updatePlan :<|> deletePlan

getPlans :: AppM [CouplePlan]
getPlans = map toCouplePlan <$> runDB (selectList [] [Asc CouplePlanDbCategory])

createPlan :: CouplePlanInput -> AppM CouplePlan
createPlan input = do
  let db = CouplePlanDb
        { couplePlanDbCategory = T.pack (couplePlanInputCategory input)
        , couplePlanDbTitle    = T.pack (couplePlanInputTitle input)
        , couplePlanDbDetail   = T.pack <$> couplePlanInputDetail input
        , couplePlanDbDone     = couplePlanInputDone input
        }
  key <- runDB $ insert db
  return (toCouplePlan (Entity key db))

updatePlan :: Int -> CouplePlanInput -> AppM CouplePlan
updatePlan planId input = do
  let key = toSqlKey (fromIntegral planId) :: Key CouplePlanDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> do
      let db = CouplePlanDb
            { couplePlanDbCategory = T.pack (couplePlanInputCategory input)
            , couplePlanDbTitle    = T.pack (couplePlanInputTitle input)
            , couplePlanDbDetail   = T.pack <$> couplePlanInputDetail input
            , couplePlanDbDone     = couplePlanInputDone input
            }
      runDB $ replace key db
      return (toCouplePlan (Entity key db))

deletePlan :: Int -> AppM NoContent
deletePlan planId = do
  let key = toSqlKey (fromIntegral planId) :: Key CouplePlanDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> runDB (delete key) >> return NoContent

-- DIARY ENTRIES

data DiaryEntry = DiaryEntry
  { diaryEntryId      :: Maybe Int
  , diaryEntryDate    :: String
  , diaryEntryMood    :: Maybe String
  , diaryEntryBody    :: String
  , diaryEntryWeather :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromJSON DiaryEntry where parseJSON = genericParseJSON (aesonOpts "diaryEntry")
instance ToJSON DiaryEntry where toJSON = genericToJSON (aesonOpts "diaryEntry")

data DiaryEntryInput = DiaryEntryInput
  { diaryEntryInputDate    :: String
  , diaryEntryInputMood    :: Maybe String
  , diaryEntryInputBody    :: String
  , diaryEntryInputWeather :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromJSON DiaryEntryInput where parseJSON = genericParseJSON (aesonOpts "diaryEntryInput")
instance ToJSON DiaryEntryInput where toJSON = genericToJSON (aesonOpts "diaryEntryInput")

toDiaryEntry :: Entity DiaryEntryDb -> DiaryEntry
toDiaryEntry (Entity key db) = DiaryEntry
  { diaryEntryId      = Just (fromIntegral (fromSqlKey key))
  , diaryEntryDate    = T.unpack (diaryEntryDbDate db)
  , diaryEntryMood    = T.unpack <$> diaryEntryDbMood db
  , diaryEntryBody    = T.unpack (diaryEntryDbBody db)
  , diaryEntryWeather = T.unpack <$> diaryEntryDbWeather db
  }

type DiaryAPI =
       "api" :> "diary" :> Get '[JSON] [DiaryEntry]
  :<|> "api" :> "diary" :> ReqBody '[JSON] DiaryEntryInput :> Post '[JSON] DiaryEntry
  :<|> "api" :> "diary" :> Capture "id" Int :> ReqBody '[JSON] DiaryEntryInput :> Put '[JSON] DiaryEntry
  :<|> "api" :> "diary" :> Capture "id" Int :> Delete '[JSON] NoContent

diaryServer :: ServerT DiaryAPI AppM
diaryServer = getDiaryEntries :<|> createDiaryEntry :<|> updateDiaryEntry :<|> deleteDiaryEntry

getDiaryEntries :: AppM [DiaryEntry]
getDiaryEntries = map toDiaryEntry <$> runDB (selectList [] [Desc DiaryEntryDbDate])

createDiaryEntry :: DiaryEntryInput -> AppM DiaryEntry
createDiaryEntry input = do
  let db = DiaryEntryDb
        { diaryEntryDbDate    = T.pack (diaryEntryInputDate input)
        , diaryEntryDbMood    = T.pack <$> diaryEntryInputMood input
        , diaryEntryDbBody    = T.pack (diaryEntryInputBody input)
        , diaryEntryDbWeather = T.pack <$> diaryEntryInputWeather input
        }
  key <- runDB $ insert db
  return (toDiaryEntry (Entity key db))

updateDiaryEntry :: Int -> DiaryEntryInput -> AppM DiaryEntry
updateDiaryEntry entryId input = do
  let key = toSqlKey (fromIntegral entryId) :: Key DiaryEntryDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> do
      let db = DiaryEntryDb
            { diaryEntryDbDate    = T.pack (diaryEntryInputDate input)
            , diaryEntryDbMood    = T.pack <$> diaryEntryInputMood input
            , diaryEntryDbBody    = T.pack (diaryEntryInputBody input)
            , diaryEntryDbWeather = T.pack <$> diaryEntryInputWeather input
            }
      runDB $ replace key db
      return (toDiaryEntry (Entity key db))

deleteDiaryEntry :: Int -> AppM NoContent
deleteDiaryEntry entryId = do
  let key = toSqlKey (fromIntegral entryId) :: Key DiaryEntryDb
  existing <- runDB $ get key
  case existing of
    Nothing -> throwError err404
    Just _  -> runDB (delete key) >> return NoContent

-- GEOCODING

data GeoResult = GeoResult
  { geoDisplayName :: String
  , geoLat         :: String
  , geoLon         :: String
  } deriving (Show, Generic)

instance ToJSON GeoResult where
  toJSON gr = object
    [ "displayName" .= geoDisplayName gr
    , "lat"         .= geoLat gr
    , "lon"         .= geoLon gr
    ]

instance FromJSON GeoResult where
  parseJSON = withObject "GeoResult" $ \o ->
    GeoResult <$> o .: "display_name" <*> o .: "lat" <*> o .: "lon"

type GeoAPI = "api" :> "geocode" :> QueryParam "q" Text :> Get '[JSON] [GeoResult]

geocode :: Maybe Text -> AppM [GeoResult]
geocode Nothing  = return []
geocode (Just q) = liftIO $ do
  let encoded = concatMap encodeChar (T.unpack q)
      url     = "https://nominatim.openstreetmap.org/search?q=" ++ encoded ++ "&format=json&limit=5"
  req <- parseRequest url
  let req' = setRequestHeader "User-Agent" ["MemoryDiary/1.0"] req
  resp <- httpLBS req'
  case decode (getResponseBody resp) :: Maybe [GeoResult] of
    Nothing  -> return []
    Just res -> return res
  where
    encodeChar ' ' = "%20"
    encodeChar ',' = "%2C"
    encodeChar '&' = "%26"
    encodeChar c   = [c]

app :: ConnectionPool -> Application
app pool =
  serve (Proxy :: Proxy FullAPI) $
    hoistServer (Proxy :: Proxy MemoryAPI)          (`runReaderT` pool) memoryServer
    :<|> hoistServer (Proxy :: Proxy ImportantDayAPI) (`runReaderT` pool) importantDayServer
    :<|> hoistServer (Proxy :: Proxy RelationshipAPI) (`runReaderT` pool) relationshipServer
    :<|> hoistServer (Proxy :: Proxy OnThisDayAPI)    (`runReaderT` pool) getOnThisDay
    :<|> hoistServer (Proxy :: Proxy NoteAPI)         (`runReaderT` pool) noteServer
    :<|> hoistServer (Proxy :: Proxy PlanAPI)         (`runReaderT` pool) planServer
    :<|> hoistServer (Proxy :: Proxy DiaryAPI)        (`runReaderT` pool) diaryServer
    :<|> hoistServer (Proxy :: Proxy StatsAPI)        (`runReaderT` pool) getStats
    :<|> hoistServer (Proxy :: Proxy GeoAPI)          (`runReaderT` pool) geocode
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
