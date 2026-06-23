{-# LANGUAGE OverloadedStrings #-}

-- | HTTP handlers for the api relationship and profile endpoints
module Handler.Relationship
  ( relationshipServer,
    profileServer,
    getRelationshipRow,
    putRelationship,
    getProfile,
    putProfile,
    getRelationship,
    upsertRelationship,
    ensurePerson,
    upsertPerson,
  )
where

import qualified Api
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Domain.Recurrence
  ( nextAnnualOccurrence,
    nextMonthiversary,
    ordinalMonths,
    ordinalYears,
  )
import Handler.Helpers (AppM, runDb)
import Models
import Servant
import Types

relationshipServer :: ServerT Api.RelationshipApi AppM
relationshipServer = getRelationshipRow :<|> putRelationship

profileServer :: ServerT Api.ProfileApi AppM
profileServer = getProfile :<|> putProfile

-- RELATIONSHIP ---------------------------------------

-- | Return the current relationship record with computed countdowns
getRelationshipRow :: AppM RelationshipRow
getRelationshipRow = do
  now <- liftIO getCurrentTime
  buildRelationshipRow now <$> getRelationship

-- | Update the relationship start date and heart colour
putRelationship :: RelationshipInput -> AppM RelationshipRow
putRelationship input = do
  upsertRelationship (riStartDate input) (riHeartColor input)
  now <- liftIO getCurrentTime
  buildRelationshipRow now <$> getRelationship

-- | Build a 'RelationshipRow' from the database record, computing countdown fields
buildRelationshipRow :: UTCTime -> Maybe (Entity RelationshipDb) -> RelationshipRow
buildRelationshipRow _ Nothing =
  RelationshipRow Nothing "heart-red" Nothing Nothing Nothing Nothing Nothing Nothing Nothing
buildRelationshipRow now (Just (Entity _ rel)) =
  case relationshipDbStartDate rel of
    Nothing ->
      RelationshipRow Nothing (relationshipDbHeartColor rel) Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Just start ->
      let annDate = nextAnnualOccurrence now start
          monthDate = nextMonthiversary now start
          annNum = ordinalYears start annDate
          monthNum = ordinalMonths start monthDate
          days = max 0 (floor (diffUTCTime now start / 86400) :: Int)
       in RelationshipRow
            { rrStartDate = Just start,
              rrHeartColor = relationshipDbHeartColor rel,
              rrDaysTogether = Just days,
              rrNextAnniversary = Just annDate,
              rrAnniversaryNumber = Just annNum,
              rrNextMonthiversary = Just monthDate,
              rrMonthiversaryNum = Just monthNum,
              rrAnniversaryDays = Just (diffDays (utctDay annDate) (utctDay now)),
              rrMonthiversaryDays = Just (diffDays (utctDay monthDate) (utctDay now))
            }

-- PROFILES ---------------------------------------

-- | Return the person profile for the given side
getProfile :: Text -> AppM ProfileRow
getProfile side = do
  validateProfileSide side
  profileRowFromDb side <$> ensurePerson side

-- | Update the person profile for the given side
putProfile :: Text -> ProfileInput -> AppM ProfileRow
putProfile side input = do
  validateProfileSide side
  upsertPerson
    side
    (piName input)
    (piDisplayMode input)
    (piPhoto input)
    (piFigureColor input)
    (piAccessory input)
    (piExpression input)
    (piBirthday input)
  profileRowFromDb side <$> ensurePerson side

-- | Convert a 'PersonProfileDb' to a 'ProfileRow' for API responses
profileRowFromDb :: Text -> PersonProfileDb -> ProfileRow
profileRowFromDb side p =
  ProfileRow
    { prSide = side,
      prName = personProfileDbName p,
      prDisplayMode = personProfileDbDisplayMode p,
      prPhoto = personProfileDbPhoto p,
      prFigureColor = personProfileDbFigureColor p,
      prAccessory = personProfileDbAccessory p,
      prExpression = personProfileDbExpression p,
      prBirthday = personProfileDbBirthday p
    }

-- | Fetch the single relationship record
getRelationship :: AppM (Maybe (Entity RelationshipDb))
getRelationship = runDb (getBy (UniqueRelationshipKey "main"))

-- | Insert or update relationship settings
upsertRelationship :: Maybe UTCTime -> Text -> AppM ()
upsertRelationship startDate heartColor =
  runDb $
    void $
      upsert
        (RelationshipDb "main" startDate heartColor)
        [ RelationshipDbStartDate =. startDate,
          RelationshipDbHeartColor =. heartColor
        ]

-- | Return a profile record
ensurePerson :: Text -> AppM PersonProfileDb
ensurePerson side = do
  validateProfileSide side
  existing <- runDb (getBy (UniquePersonSide side))
  case existing of
    Just (Entity _ profile) ->
      pure profile
    Nothing -> do
      let fallback = defaultPerson side
      runDb (insert_ fallback)
      pure fallback

-- | Insert or update all profile fields
upsertPerson :: Text -> Text -> Text -> Maybe Text -> Text -> Text -> Text -> Maybe UTCTime -> AppM ()
upsertPerson side name mode photo color accessory expression birthday = do
  validateProfileSide side
  runDb $
    void $
      upsert
        (PersonProfileDb side name mode photo color accessory expression birthday)
        [ PersonProfileDbName =. name,
          PersonProfileDbDisplayMode =. mode,
          PersonProfileDbPhoto =. photo,
          PersonProfileDbFigureColor =. color,
          PersonProfileDbAccessory =. accessory,
          PersonProfileDbExpression =. expression,
          PersonProfileDbBirthday =. birthday
        ]

-- | Profiles are a fixed pair of records identified by "left" and "right" sides
validateProfileSide :: Text -> AppM ()
validateProfileSide side
  | side `elem` ["left", "right"] = pure ()
  | otherwise = throwError err400 {errBody = "Profile side must be left or right."}

-- | Convert a Persistent entity to the JSON row type
defaultPerson :: Text -> PersonProfileDb
defaultPerson side =
  PersonProfileDb
    { personProfileDbSide = side,
      personProfileDbName = "",
      personProfileDbDisplayMode = "figure",
      personProfileDbPhoto = Nothing,
      personProfileDbFigureColor = if side == "left" then "figure-red" else "figure-blue",
      personProfileDbAccessory = if side == "left" then "flower" else "hat",
      personProfileDbExpression = if side == "left" then "smile" else "laugh",
      personProfileDbBirthday = Nothing
    }
