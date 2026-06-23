module Domain.Aggregation
  ( parseDT
  , durMins
  , topN
  , activityBy
  , computeStats
  ) where

import Data.List (group, maximumBy, nub, sort, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Types

parseDT :: String -> Maybe UTCTime
parseDT = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M"

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
