{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared HTTP-layer utilities and the application monad used across all handler modules.

module Handler.Helpers
  ( -- Application monad
    AppM,
    runDb,

    -- Entity key conversion
    fromIntKey,
    toIntKey,

    -- HTTP error helpers
    badRequest,
    notFound,
    validateInput,

    -- Soft-delete support
    moveToTrash,
    encodeToText,

    -- JSON name extraction
    JustName (..),
    extractName,

    -- Collection utilities
    dedupeText,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Aeson (FromJSON (..), eitherDecodeStrict, withObject, (.:))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Database.Persist (insert_)
import Database.Persist.Postgresql (ConnectionPool)
import Database.Persist.Sql (Key, SqlBackend, SqlPersistT, ToBackendKey, fromSqlKey, runSqlPool, toSqlKey)
import Domain.Validation (ValidationError, renderValidationError)
import Models (TrashItemDb (..))
import Servant (Handler, err400, err404, errBody, throwError)

-- | The application monad: a 'Handler' with read-only access to the database connection pool
type AppM = ReaderT ConnectionPool Handler

-- | Run a Persistent query against the pool held in the 'AppM' environment
runDb :: SqlPersistT IO a -> AppM a
runDb query = do
  pool <- ask
  liftIO $ runSqlPool query pool

-- | Convert a plain 'Int' to a Persistent entity key
fromIntKey :: (ToBackendKey SqlBackend record) => Int -> Key record
fromIntKey = toSqlKey . fromIntegral

-- | Convert a Persistent entity key to a plain 'Int'
toIntKey :: (ToBackendKey SqlBackend record) => Key record -> Int
toIntKey = fromIntegral . fromSqlKey

-- | Throw a 400 Bad Request with the given message body
badRequest :: Text -> AppM a
badRequest message =
  throwError err400 {errBody = BSL.fromStrict (TE.encodeUtf8 message)}

-- | Throw a 404 Not Found
notFound :: AppM a
notFound = throwError err404

-- | Run a validation result, throwing 400 on the first failure
validateInput :: Either ValidationError () -> AppM ()
validateInput = either (badRequest . renderValidationError) pure

-- | Serialise any JSON-encodable value to 'Text'
encodeToText :: (A.ToJSON a) => a -> Text
encodeToText = TE.decodeUtf8 . BSL.toStrict . A.encode

-- | Soft-delete an item by persisting its JSON payload in the trash table
moveToTrash :: (A.ToJSON a) => Text -> a -> AppM ()
moveToTrash kind item = do
  now <- liftIO getCurrentTime
  runDb (insert_ (TrashItemDb kind (encodeToText item) now))

-- | Newtype used to extract a display name from JSON payload
newtype JustName = JustName Text

instance FromJSON JustName where
  parseJSON = withObject "JustName" $ \o ->
    JustName <$> (o .: "title" <|> o .: "name")

-- | Pull the first available name or title field out of a stored JSON payload
extractName :: Text -> Text
extractName payload =
  case eitherDecodeStrict (TE.encodeUtf8 payload) of
    Right (JustName t) -> t
    Left _ -> "—"

-- | Remove duplicate 'Text' values, keeping the first occurrence of each
dedupeText :: [Text] -> [Text]
dedupeText = nub
