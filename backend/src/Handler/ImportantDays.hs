{-# LANGUAGE OverloadedStrings #-}

--   Manages manual important days and includes profile birthdays in list results

-- | HTTP handlers for the api important-days endpoints
module Handler.ImportantDays
  ( importantDayServer,
    listImportantDays,
    createImportantDay,
    getImportantDay,
    updateImportantDay,
    deleteImportantDay,
    importantDayRowFromEntity,
  )
where

import qualified Api
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Time (UTCTime, diffDays, getCurrentTime, toGregorian, utctDay)
import Database.Persist
import Domain.Recurrence (nextAnnualOccurrence)
import Domain.Validation (cleanMaybeText, validateImportantDayInputFields)
import Handler.Helpers
import Models
import Servant
import Types

importantDayServer :: ServerT Api.ImportantDayApi AppM
importantDayServer =
  listImportantDays
    :<|> createImportantDay
    :<|> updateImportantDay
    :<|> deleteImportantDay

-- | Return manually entered important days and profile birthdays
listImportantDays :: AppM [ImportantDayRow]
listImportantDays = do
  now <- liftIO getCurrentTime
  days <- runDb (selectList [ImportantDayDbKind ==. "manual"] [Asc ImportantDayDbDate])
  profiles <- runDb (selectList [PersonProfileDbBirthday !=. Nothing] [Asc PersonProfileDbSide])
  pure (map (importantDayRowFromEntity now) days ++ mapMaybeBirthday now profiles)
  where
    mapMaybeBirthday now = foldr (\entity rows -> maybe rows (: rows) (birthdayRowFromEntity now entity)) []

-- | Create a new manually entered important day
createImportantDay :: ImportantDayInput -> AppM ImportantDayRow
createImportantDay input = do
  validateImportantDay input
  dayId <-
    runDb $
      insert $
        ImportantDayDb (T.strip (idiTitle input)) (idiDate input) (cleanMaybeText (idiNote input)) "manual"
  getImportantDay (toIntKey dayId)

-- | Replace all fields of an existing important day
updateImportantDay :: Int -> ImportantDayInput -> AppM NoContent
updateImportantDay rawId input = do
  validateImportantDay input
  let dayId = fromIntKey rawId :: ImportantDayDbId
  existing <- runDb (get dayId)
  case existing of
    Nothing -> notFound
    Just _ -> do
      runDb $
        replace dayId $
          ImportantDayDb (T.strip (idiTitle input)) (idiDate input) (cleanMaybeText (idiNote input)) "manual"
      pure NoContent

-- | Soft-delete a manually entered important day
--   The trash insert and the row delete run in one transaction
deleteImportantDay :: Int -> AppM NoContent
deleteImportantDay rawId = do
  row <- getImportantDay rawId
  runDb $ do
    now <- liftIO getCurrentTime
    insert_ (TrashItemDb "important-day" (encodeToText (ImportantDayInput (idrTitle row) (idrDate row) (idrNote row))) now)
    delete (fromIntKey rawId :: ImportantDayDbId)
  pure NoContent

-- | Fetch a single important day by numeric id
getImportantDay :: Int -> AppM ImportantDayRow
getImportantDay rawId = do
  now <- liftIO getCurrentTime
  mDay <- runDb (getEntity (fromIntKey rawId :: ImportantDayDbId))
  case mDay of
    Nothing -> notFound
    Just day -> pure (importantDayRowFromEntity now day)

-- | Validate an 'ImportantDayInput'
validateImportantDay :: ImportantDayInput -> AppM ()
validateImportantDay = validateInput . validateImportantDayInputFields

-- | Convert a Persistent entity to the JSON row type
importantDayRowFromEntity :: UTCTime -> Entity ImportantDayDb -> ImportantDayRow
importantDayRowFromEntity now (Entity dayId day) =
  recurringRow
    now
    (Just (toIntKey dayId))
    (importantDayDbTitle day)
    (importantDayDbDate day)
    (importantDayDbNote day)
    "manual"
    Nothing

-- | Convert a profile birthday to a read-only important-day row
birthdayRowFromEntity :: UTCTime -> Entity PersonProfileDb -> Maybe ImportantDayRow
birthdayRowFromEntity now (Entity _ profile) =
  case personProfileDbBirthday profile of
    Nothing ->
      Nothing
    Just birthday ->
      Just (recurringRow now Nothing (personProfileDbName profile) birthday Nothing "birthday" (Just (personProfileDbSide profile)))

-- | Add the backend-owned annual recurrence projection to an API row
recurringRow :: UTCTime -> Maybe Int -> T.Text -> UTCTime -> Maybe T.Text -> T.Text -> Maybe T.Text -> ImportantDayRow
recurringRow now rowId title source note kind owner =
  let occurrence = nextAnnualOccurrence now source
      (_, month, day) = toGregorian (utctDay source)
   in ImportantDayRow
        { idrId = rowId,
          idrTitle = title,
          idrDate = source,
          idrNote = note,
          idrKind = kind,
          idrOwner = owner,
          idrNextOccurrence = occurrence,
          idrDaysUntil = diffDays (utctDay occurrence) (utctDay now),
          idrMonth = month,
          idrDay = day
        }
