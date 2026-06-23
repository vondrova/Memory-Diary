{-# LANGUAGE OverloadedStrings #-}

-- | Pure predicate-based filtering of memory rows.
module Domain.Filter
  ( applyMemoryFilters
  , matchesFilters
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Types

-- | Keep only the memories that satisfy all supplied filter criteria.
--   'Nothing' for any criterion means no constraint on that field.
applyMemoryFilters
  :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
  -> [Memory] -> [Memory]
applyMemoryFilters mq mtag mloc mfrom mto =
  filter (matchesFilters mq mtag mloc mfrom mto)

-- | Test whether a single memory satisfies every supplied criterion.
matchesFilters
  :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
  -> Memory -> Bool
matchesFilters mq mtag mloc mfrom mto mem =
  checkQ && checkTag && checkLoc && checkFrom && checkTo
  where
    lower = T.toLower
    checkQ = case mq of
      Nothing -> True
      Just q  ->
        T.isInfixOf (lower q) (lower (T.pack (memoryTitle mem)))
        || maybe False (T.isInfixOf (lower q) . lower . T.pack) (memoryDescription mem)
    checkTag = case mtag of
      Nothing  -> True
      Just tag -> T.unpack tag `elem` memoryTags mem
    checkLoc = case mloc of
      Nothing  -> True
      Just loc -> maybe False (T.isInfixOf (lower loc) . lower . T.pack) (memoryLocation mem)
    checkFrom = case mfrom of
      Nothing   -> True
      Just from -> T.unpack from <= memoryTimeFrom mem
    checkTo = case mto of
      Nothing -> True
      Just to -> memoryTimeTo mem <= T.unpack to
