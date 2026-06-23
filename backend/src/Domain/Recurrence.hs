module Domain.Recurrence
  ( parseDay
  , formatDay
  , computeNextOccurrence
  , monthDay
  ) where

import Data.Time (formatTime)
import Data.Time.Calendar (Day, diffDays, fromGregorian, toGregorian)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"

computeNextOccurrence :: Day -> String -> (String, Int, Int, Int)
computeNextOccurrence today dateStr =
  case parseDay dateStr of
    Nothing -> (dateStr, 0, 0, 0)
    Just d  ->
      let (_, m, dd)   = toGregorian d
          (yr, _, _)   = toGregorian today
          thisYearDate = fromGregorian yr m dd
          nextDate     = if today <= thisYearDate then thisYearDate
                         else fromGregorian (yr + 1) m dd
          days         = fromIntegral (diffDays nextDate today)
      in (formatDay nextDate, days, fromIntegral m, fromIntegral dd)

monthDay :: String -> String
monthDay s = take 5 (drop 5 s)
