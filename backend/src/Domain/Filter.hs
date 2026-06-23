{-# LANGUAGE OverloadedStrings #-}

-- | Pure predicate-based filtering of memory rows
module Domain.Filter
  ( applyMemoryFilters, -- keep only the rows that satisfy all supplied filter criteria
    matchesFilters, -- test whether a single memory row satisfies every supplied criterion
    parseUtcTime, -- try to parse a date/time string in three formats, in order: YYYY-MM-DDThh:mm, YYYY-MM-DDThh:mm:ss, YYYY-MM-DD (midnight)
    validFilterRange, -- from <= to
  )
where

import Control.Applicative ((<|>)) -- for trying multiple parse formats in parseUtcTime
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Types

-- | Keep only the rows that satisfy all supplied filter criteria.
--   'Nothing' for any criterion means no constraint on that field.
applyMemoryFilters ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  [MemoryRow] ->
  [MemoryRow]
applyMemoryFilters mq mtag mloc mfrom mto =
  filter (matchesFilters mq mtag mloc mfrom mto)

-- | Test whether the supplied date range is valid (from <= to), unparseable values are ignored
validFilterRange :: Maybe Text -> Maybe Text -> Bool
validFilterRange mfrom mto =
  case (mfrom >>= parseUtcTime, mto >>= parseUtcTime) of
    (Just from, Just to) ->
      from <= to
    _ ->
      True

-- | Test whether a single memory row satisfies every supplied criterion
matchesFilters ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  MemoryRow ->
  Bool
matchesFilters mq mtag mloc mfrom mto mem =
  checkQ && checkTag && checkLoc && checkFrom && checkTo
  where
    lower = T.toLower -- case-insensitive matching
    checkQ = case mq of -- free-text search across title and description
      Nothing -> True
      Just q ->
        T.isInfixOf (lower q) (lower (mrTitle mem)) -- search in title
          || maybe False (T.isInfixOf (lower q) . lower) (mrDescription mem) -- search in description (search for substring)
    checkTag = case mtag of -- filter by tag
      Nothing -> True
      Just tag -> any (T.isInfixOf (lower tag) . lower) (mrTags mem)
    checkLoc = case mloc of -- filter by location
      Nothing -> True
      Just loc -> maybe False (T.isInfixOf (lower loc) . lower) (mrLocation mem)
    checkFrom = case mfrom of -- filter by start of date range
      Nothing -> True
      Just from -> maybe True (mrTimeFrom mem >=) (parseUtcTime from)
    checkTo = case mto of -- filter by end of date range
      Nothing -> True
      Just to -> maybe True (mrTimeTo mem <=) (parseUtcTime to)

-- | Try to parse a date/time string in three formats, in order:
--   @YYYY-MM-DDThh:mm@, @YYYY-MM-DDThh:mm:ss@, @YYYY-MM-DD@ (midnight)
parseUtcTime :: Text -> Maybe UTCTime
parseUtcTime t =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" (T.unpack t)
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack t)
    <|> parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t)
