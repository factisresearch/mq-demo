{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}

module Mgw.Util.TimeSpan
    ( TimeSpan(..) -- don't use exported datacons
    , zeroTime
    , microseconds, microseconds'
    , milliseconds, milliseconds'
    , seconds, seconds', doubleSeconds
    , asMicroseconds, asMilliseconds, asSeconds, asMinutes
    , asSeconds'
    , asHours, asDays
    , diffTimeSpan, plusTimeSpan, multiplyTimeSpan, diffClockTimes
    , picoseconds, nanoseconds, minutes, hours, days
    , displayTimeSpan, parseTimeSpan, parseTimeSpanWithError
    , isPositiveTimeSpan
    , nominalDiffTimeSpan
    )
where

#include "src/macros.h"

import Data.Char
import Data.Time.Clock (NominalDiffTime)
import Control.DeepSeq
import System.Time hiding (diffClockTimes)
import Safe
import System.Random

-- | Represents a time span with microsecond resolution.
newtype TimeSpan = TimeSpan { unTimeSpan :: Integer }
    deriving (Eq, Ord, Num, Random, NFData)

instance Show TimeSpan where
    showsPrec _ (TimeSpan t) =
        if us == 0 && ms == 0 && s == 0 && min == 0 && h == 0 && days == 0
           then showString "0us"
           else possiblyShow days "days" .
                possiblyShow h "h" .
                possiblyShow min "min" .
                possiblyShow s "s" .
                possiblyShow ms "ms" .
                possiblyShow us "us"
            where
              possiblyShow i unit =
                  if (i == 0) then id else (showsPrec 0 i . showString unit)
              us = t `mod` 1000
              ms = t `div` 1000 `mod` 1000
              s = t `div` (1000^2) `mod` 60
              min = t `div` (1000^2 * 60) `mod` 60
              h = t `div` (1000^2 * 60^2) `mod` 24
              days = t `div` (1000^2 * 60^2 * 24)

zeroTime :: TimeSpan
zeroTime = TimeSpan 0

isPositiveTimeSpan :: TimeSpan -> Bool
isPositiveTimeSpan (TimeSpan t) = (t > 0)

nominalDiffTimeSpan :: NominalDiffTime -> TimeSpan
nominalDiffTimeSpan dt = nanoseconds (truncate (dt * 1000 * 1000 * 1000))

picoseconds :: Integer -> TimeSpan
picoseconds = TimeSpan . (`div` (1000 * 1000))

nanoseconds :: Integer -> TimeSpan
nanoseconds = TimeSpan . (`div` 1000)

microseconds :: Int -> TimeSpan
microseconds = TimeSpan . fromIntegral

microseconds' :: Integer -> TimeSpan
microseconds' = TimeSpan

milliseconds :: Int -> TimeSpan
milliseconds i = TimeSpan (fromIntegral i * 1000)

milliseconds' :: Integer -> TimeSpan
milliseconds' i = TimeSpan (i * 1000)

seconds :: Int -> TimeSpan
seconds i = TimeSpan (fromIntegral i * 1000000)

seconds' :: Integer -> TimeSpan
seconds' i = TimeSpan (i * 1000000)

doubleSeconds :: Double -> TimeSpan
doubleSeconds d = TimeSpan (round $ d * 1000000)

minutes :: Int -> TimeSpan
minutes i = TimeSpan (fromIntegral i * 1000000 * 60)

hours :: Int -> TimeSpan
hours i = TimeSpan (fromIntegral i * 1000000 * 60 * 60)

days :: Int -> TimeSpan
days i = TimeSpan (fromIntegral i * 24 * 1000000 * 60 * 60)

asMicroseconds :: Integral i => TimeSpan -> i
asMicroseconds = fromIntegral . unTimeSpan

asMilliseconds :: Integral i => TimeSpan -> i
asMilliseconds (TimeSpan t) = fromIntegral (t `div` 1000)

asSeconds :: Integral i => TimeSpan -> i
asSeconds (TimeSpan t) = fromIntegral (t `div` 1000000)

asSeconds' :: Fractional a => TimeSpan -> a
asSeconds' (TimeSpan t) = fromInteger t / (1000 * 1000)

asMinutes :: Integral i => TimeSpan -> i
asMinutes (TimeSpan t) = fromIntegral (t `div` (1000000 * 60))

asHours :: Integral i => TimeSpan -> i
asHours (TimeSpan t) = fromIntegral (t `div` (1000000 * 60 * 60))

asDays :: TimeSpan -> Double
asDays (TimeSpan t) = (fromIntegral t) / (1000000 * 60 * 60 * 24)

diffTimeSpan :: TimeSpan -> TimeSpan -> TimeSpan
diffTimeSpan (TimeSpan t1) (TimeSpan t0) = TimeSpan (t1 - t0)

plusTimeSpan :: TimeSpan -> TimeSpan -> TimeSpan
plusTimeSpan (TimeSpan t1) (TimeSpan t0) = TimeSpan (t1 + t0)

multiplyTimeSpan :: Double -> TimeSpan -> TimeSpan
multiplyTimeSpan x (TimeSpan t) = TimeSpan . round $ x * fromIntegral t

diffClockTimes :: ClockTime -> ClockTime -> TimeSpan
diffClockTimes (TOD s1 p1) (TOD s0 p0) = (picoseconds p1 `plusTimeSpan` seconds' s1) `diffTimeSpan`
                                         (picoseconds p0 `plusTimeSpan` seconds' s0)

displayTimeSpan :: TimeSpan -> String
displayTimeSpan (TimeSpan us)
    | us < 1000 = show us ++ "us"
    | ms < 10000 = show ms ++ "ms"
    | s < 180 = show s ++ "s"
    | m < 90 = show m ++ "min"
    | h < 48 = show h ++ "h"
    | otherwise = show d ++ " days"
    where
      ms = us `div` 1000
      s = ms `div` 1000
      m = s `div` 60
      h = m `div` 60
      d = h `div` 24

parseTimeSpan :: Monad m => String -> m TimeSpan
parseTimeSpan str =
    let (valStr, unitWithSpaces) = span isDigit str
        unitStr = dropWhile isSpace unitWithSpaces
    in case readMay valStr of
         Nothing ->
             fail ("Could not parse time value " ++ show valStr ++ " in TimeSpan: " ++ show str)
         Just val ->
             case unitStr of
               [] -> return $ microseconds val
               "us" -> return $ microseconds val
               "ms" -> return $ milliseconds val
               "s" -> return $ seconds val
               "min" -> return $ minutes val
               "h" -> return $ hours val
               "d" -> return $ days val
               _ ->
                   fail ("Could not parse TimeSpan unit " ++ show unitStr ++ " in " ++ show str)

parseTimeSpanWithError :: String -> TimeSpan
parseTimeSpanWithError str =
    case parseTimeSpan str of
      Just t -> t
      Nothing -> safeError ("Could not parse " ++ show str ++ " as a time span")
