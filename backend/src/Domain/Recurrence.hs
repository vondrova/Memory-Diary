-- | Date-recurrence helpers for important days and anniversaries.
module Domain.Recurrence
  ( parseDay
  , formatDay
  , computeNextOccurrence
  , monthDay
  ) where

import Data.Time (formatTime)
import Data.Time.Calendar (Day, diffDays, fromGregorian, toGregorian)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

-- | Parse an ISO 8601 date string (@YYYY-MM-DD@) into a 'Day'.
parseDay :: String -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- | Format a 'Day' as an ISO 8601 date string (@YYYY-MM-DD@).
formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"

-- | Given today's date and a stored date string, return
--   @(nextOccurrenceDate, daysUntil, month, day)@.
--   If the anniversary has already passed this year, the next occurrence is next year.
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

-- | Extract the @MM-DD@ portion of an ISO date or datetime string,
--   used to match anniversaries regardless of the year.
monthDay :: String -> String
monthDay s = take 5 (drop 5 s)
