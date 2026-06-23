{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp
  , Memory (..)
  , Stats (..)
  , durMins
  , topN
  , splitComma
  , joinComma
  , computeNextOccurrence
  , parseDay
  , monthDay
  , computeStats
  ) where

import Control.Monad (zipWithM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (decode)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, formatTime, getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale)
import Domain.Aggregation (computeStats, durMins, topN)
import Domain.Recurrence (computeNextOccurrence, monthDay, parseDay, formatDay)
import Models
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest, setRequestHeader)
import Network.Wai (Application)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Multipart
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import Types

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

type ImportantDayAPI =
       "api" :> "important-days" :> Get '[JSON] [ImportantDay]
  :<|> "api" :> "important-days" :> ReqBody '[JSON] ImportantDayInput :> Post '[JSON] ImportantDay
  :<|> "api" :> "important-days" :> Capture "id" Int :> ReqBody '[JSON] ImportantDayInput :> Put '[JSON] ImportantDay
  :<|> "api" :> "important-days" :> Capture "id" Int :> Delete '[JSON] NoContent

type RelationshipAPI =
       "api" :> "relationship" :> Get '[JSON] (Maybe Relationship)
  :<|> "api" :> "relationship" :> ReqBody '[JSON] RelationshipInput :> Put '[JSON] Relationship

type OnThisDayAPI = "api" :> "on-this-day" :> Get '[JSON] OnThisDay

type NoteAPI =
       "api" :> "notes" :> Get '[JSON] [CoupleNote]
  :<|> "api" :> "notes" :> ReqBody '[JSON] CoupleNoteInput :> Post '[JSON] CoupleNote
  :<|> "api" :> "notes" :> Capture "id" Int :> ReqBody '[JSON] CoupleNoteInput :> Put '[JSON] CoupleNote
  :<|> "api" :> "notes" :> Capture "id" Int :> Delete '[JSON] NoContent

type PlanAPI =
       "api" :> "plans" :> Get '[JSON] [CouplePlan]
  :<|> "api" :> "plans" :> ReqBody '[JSON] CouplePlanInput :> Post '[JSON] CouplePlan
  :<|> "api" :> "plans" :> Capture "id" Int :> ReqBody '[JSON] CouplePlanInput :> Put '[JSON] CouplePlan
  :<|> "api" :> "plans" :> Capture "id" Int :> Delete '[JSON] NoContent

type DiaryAPI =
       "api" :> "diary" :> Get '[JSON] [DiaryEntry]
  :<|> "api" :> "diary" :> ReqBody '[JSON] DiaryEntryInput :> Post '[JSON] DiaryEntry
  :<|> "api" :> "diary" :> Capture "id" Int :> ReqBody '[JSON] DiaryEntryInput :> Put '[JSON] DiaryEntry
  :<|> "api" :> "diary" :> Capture "id" Int :> Delete '[JSON] NoContent

type StatsAPI = "api" :> "stats" :> Get '[JSON] Stats

type GeoAPI = "api" :> "geocode" :> QueryParam "q" Text :> Get '[JSON] [GeoResult]

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

-- MEMORY HANDLERS

memoryServer :: ServerT MemoryAPI AppM
memoryServer = getMemories :<|> createMemory :<|> updateMemory :<|> deleteMemory

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
    Just _  -> runDB (delete key) >> return NoContent

-- IMPORTANT DAY HANDLERS

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
    Just _  -> runDB (delete key) >> return NoContent

-- RELATIONSHIP HANDLERS

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
    []               -> runDB $ insert db
    (Entity k _ : _) -> runDB (replace k db) >> return k
  return (toRelationship today (Entity key db))

-- ON-THIS-DAY HANDLER

getOnThisDay :: AppM OnThisDay
getOnThisDay = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let todayMD = formatTime defaultTimeLocale "%m-%d" today

  memEntities <- runDB $ selectList [] []
  let todayMems = filter (\m -> monthDay (memoryTimeFrom m) == todayMD) (map toMemory memEntities)

  dayEntities <- runDB $ selectList [] []
  let todayDays = filter (\d -> monthDay (importantDayDate d) == todayMD) (map (toImportantDay today) dayEntities)

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

-- NOTE HANDLERS

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

-- PLAN HANDLERS

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

-- DIARY HANDLERS

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

-- STATS HANDLER

getStats :: AppM Stats
getStats = do
  entities <- runDB $ selectList [] []
  return $ computeStats (map toMemory entities)

-- GEOCODING HANDLER

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

-- PHOTO HANDLER

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

-- APPLICATION

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
