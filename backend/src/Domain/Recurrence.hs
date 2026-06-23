{-# LANGUAGE OverloadedStrings #-}

-- | Pure date arithmetic for recurring annual and monthly events
module Domain.Recurrence
  ( nextAnnualOccurrence,
    nextMonthiversary,
    ordinalYears, -- number of complete years between start and occurrence, used for "Xth anniversary" labels
    ordinalMonths, -- number of complete months between start and occurrence, used for "Xth monthiversary" labels
    diffMonths, -- signed difference in whole months between two dates, used internally by ordinalMonths and nextMonthiversary  
  )
where

import Data.Time
  ( Day,
    UTCTime (..),
    addGregorianMonthsClip,
    fromGregorian,
    toGregorian,
    utctDay,
  )

-- | Return the nearest future calendar date on which the annual anniversary falls
nextAnnualOccurrence :: UTCTime -> UTCTime -> UTCTime
nextAnnualOccurrence now start =
  let today = utctDay now -- without time component
      (_, month, day) = toGregorian (utctDay start)
      (year, _, _) = toGregorian today 
      thisYear = fromGregorian year month day 
      nextDay = if thisYear >= today then thisYear else fromGregorian (year + 1) month day -- if this year's occurrence has passed, use next year
   in UTCTime nextDay 0 -- without time component

-- | Return the nearest future calendar date on which the monthly anniversary falls
nextMonthiversary :: UTCTime -> UTCTime -> UTCTime
nextMonthiversary now start =
  let today = utctDay now
      startDay = utctDay start
      elapsedMonths = max 0 (diffMonths startDay today) -- how many whole months have passed since the start date
      candidate = addGregorianMonthsClip elapsedMonths startDay -- the most recent monthiversary (could be in the past if the day of month doesn't exist in this month - help from AI here)
      nextDay = if candidate >= today then candidate else addGregorianMonthsClip (elapsedMonths + 1) startDay -- if the most recent monthiversary has passed, use the next one
   in UTCTime nextDay 0

-- | Number of complete years between start and occurrence
ordinalYears :: UTCTime -> UTCTime -> Integer
ordinalYears start occurrence =
  let (startYear, _, _) = toGregorian (utctDay start)
      (occYear, _, _) = toGregorian (utctDay occurrence)
   in max 0 (occYear - startYear)

-- | Number of complete months between start and occurrence
ordinalMonths :: UTCTime -> UTCTime -> Integer
ordinalMonths start occurrence =
  max 0 (diffMonths (utctDay start) (utctDay occurrence))

-- | Signed difference in whole months between two days
diffMonths :: Day -> Day -> Integer
diffMonths start end =
  let (sy, sm, _) = toGregorian start
      (ey, em, _) = toGregorian end
   in (ey - sy) * 12 + toInteger em - toInteger sm
