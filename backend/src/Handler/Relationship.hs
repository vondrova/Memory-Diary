{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the /api/relationship and /api/on-this-day endpoints.
module Handler.Relationship
  ( relationshipServer
  , getOnThisDay
  ) where

import Api (RelationshipAPI, OnThisDayAPI)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Time (getCurrentTime, formatTime, utctDay)
import Data.Time.Format (defaultTimeLocale)
import Domain.Recurrence (monthDay, formatDay)
import Handler.Helpers (AppM, runDB)
import Models
import Servant
import Types
import qualified Data.Text as T

relationshipServer :: ServerT RelationshipAPI AppM
relationshipServer = getRelationship :<|> upsertRelationship

-- | Return the single relationship record, or Nothing if none exists yet.
getRelationship :: AppM (Maybe Relationship)
getRelationship = do
  today    <- liftIO $ utctDay <$> getCurrentTime
  entities <- runDB $ (selectList [] [LimitTo 1] :: ReaderT SqlBackend IO [Entity RelationshipDb])
  return $ case entities of
    []      -> Nothing
    (e : _) -> Just (toRelationship today e)

-- | Create or replace the relationship record.
upsertRelationship :: RelationshipInput -> AppM Relationship
upsertRelationship input = do
  today    <- liftIO $ utctDay <$> getCurrentTime
  entities <- runDB $ (selectList [] [LimitTo 1] :: ReaderT SqlBackend IO [Entity RelationshipDb])
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

-- | Memories and important days whose anniversary falls on today's date.
getOnThisDay :: ServerT OnThisDayAPI AppM
getOnThisDay = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let todayMD = formatTime defaultTimeLocale "%m-%d" today

  memEntities <- runDB $ selectList [] []
  let todayMems = filter (\m -> monthDay (memoryTimeFrom m) == todayMD) (map toMemory memEntities)

  dayEntities <- runDB $ selectList [] []
  let todayDays = filter (\d -> monthDay (importantDayDate d) == todayMD) (map (toImportantDay today) dayEntities)

  relEntities <- runDB $ (selectList [] [LimitTo 1] :: ReaderT SqlBackend IO [Entity RelationshipDb])
  let mRel   = case relEntities of
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
