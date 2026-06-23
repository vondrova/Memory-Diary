-- | Pure aggregation and statistics over memory rows
module Domain.Aggregation
  ( computeStats, -- make the whole Stats summary from a list of MemoryRow
    memDurationMinutes, -- duration of one memory in minutes
    countItems, -- occurences of each distinct value in a list
    topByCount, -- top 10 items by count, descending
  )
where

import Data.List (maximumBy, sortBy) -- for longest memory and sorting stats
import qualified Data.Map.Strict as Map -- for counting occurrences of tags, locations, etc.
import Data.Maybe (mapMaybe) -- for counting non-null locations
import Data.Ord (Down (..), comparing) -- for sorting by count descending
import qualified Data.Text as T -- for formatting dates in stats
import Data.Time -- for computing durations and formatting dates in stats
import Models (Stats (..))
import Types

-- | Aggregate a list of memory rows into a 'Stats' summary
computeStats :: [MemoryRow] -> Stats
computeStats rows =
  let durations = map memDurationMinutes rows
      total = sum durations
      count = length rows
      photos = sum (map (length . mrPhotos) rows)
      places = length $ countItems $ mapMaybe mrLocation rows
      avgMins = if count == 0 then 0 else total `div` count
      longest = longestMemory rows
   in Stats
        { statsTotalMinutes = total,
          statsMemoryCount = count,
          statsPhotoCount = photos,
          statsVisitedPlaces = places,
          statsAverageMinutes = avgMins,
          statsLongestTitle = fst <$> longest,
          statsLongestMinutes = maybe 0 snd longest,
          statsTopTags = topByCount (countItems (concatMap mrTags rows)),
          statsTopLocations = topByCount (countItems (mapMaybe mrLocation rows)),
          statsMonthlyActivity =
            sortBy (comparing fst) $
              countItems $
                map (T.pack . formatTime defaultTimeLocale "%Y-%m" . mrTimeFrom) rows,
          statsDailyActivity =
            sortBy (comparing fst) $
              countItems $
                map (T.pack . formatTime defaultTimeLocale "%Y-%m-%d" . mrTimeFrom) rows
        }

-- | Duration of a memory in whole minutes
memDurationMinutes :: MemoryRow -> Int
memDurationMinutes mem =
  round (diffUTCTime (mrTimeTo mem) (mrTimeFrom mem) / 60 :: NominalDiffTime)

-- | Return the title and duration (minutes) of the longest memory, or Nothing
longestMemory :: [MemoryRow] -> Maybe (T.Text, Int)
longestMemory [] = Nothing
longestMemory rows =
  let pairs = map (\r -> (mrTitle r, memDurationMinutes r)) rows
   in Just (maximumBy (comparing snd) pairs)

-- | Count occurrences of each distinct value, returning an unsorted association list
countItems :: (Ord a) => [a] -> [(a, Int)]
countItems =
  Map.toList . foldl (\m x -> Map.insertWith (+) x 1 m) Map.empty

-- | Return the top 10 items by count, descending
topByCount :: [(a, Int)] -> [(a, Int)]
topByCount =
  take 10 . sortBy (comparing (Down . snd))
