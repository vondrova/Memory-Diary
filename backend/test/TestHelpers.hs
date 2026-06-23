{-# LANGUAGE OverloadedStrings #-}

-- | Shared test helpers used across all spec modules.
module TestHelpers
  ( atDay,
    atTimeSec,
    atHour,
  )
where

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)

-- | Build a 'UTCTime' at midnight on the given date
atDay :: Integer -> Int -> Int -> UTCTime
atDay year month day =
  UTCTime (fromGregorian year month day) (secondsToDiffTime 0)

-- | Build a 'UTCTime' at an exact clock time (hours, minutes, seconds)
atTimeSec :: Integer -> Int -> Int -> Int -> Int -> Int -> UTCTime
atTimeSec year month day h m s =
  UTCTime (fromGregorian year month day) (secondsToDiffTime (fromIntegral (h * 3600 + m * 60 + s)))

-- | Build a 'UTCTime' at the start of a given hour (minutes and seconds = 0)
atHour :: Integer -> Int -> Int -> Int -> UTCTime
atHour year month day h =
  atTimeSec year month day h 0 0
