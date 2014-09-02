{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Mgw.Model.XsdTypes
    ( XsdDateTime(..), XsdDate(..), XsdString
    , parseXsdDateTime, unsafeParseXsdDateTime, formatXsdDateTime, formatXsdDateTimeForHuman
    , formatXsdDateTimeForHumanShortWithSecs, formatXsdDateTimeGen
    , parseXsdDate, formatXsdDate, unsafeParseXsdDate, formatXsdDateAsShort
    , getCurrentXsdDateTime, getCurrentXsdDate, getNextWorkday
    , xsdDateTimeToUTCTime, xsdDateTimeToLocalTime, utcTimeToXsdDateTime
    , xsdDateTimeToSecondsSinceEpoch, secondsSinceEpochToXsdDateTime
    , secondsSince2001ToXsdDateTime, xsdDateTimeToSecondsSince2001
    , formatXsdDateTimeForHumanTable, clockTimeToXsdDateTime, diffXsdDateTime
    , formatXsdDateTimeForHumanTableMillis, formatXsdDateTimeDayMonth
    , xsdDateTimeToNumber, xsdDateTimeToNumberWithFormat, addXsdDate
    , addXsdDateTime, xsdDateTimeToDate, xsdDateToDateTime, formatXsdDateForHuman
    , minDate, minDateTime, formatXsdDateTimeForHumanShort, formatHighPrecisionTime
    , formatXsdDateTimeAsUtcInteger, subXsdDateTime, getXsdDayOfWeek
    , truncateToSecond, truncateToMinute, truncateToHour, truncateToDay, truncateToMonth
    , truncateTo12h, truncateTo6h, truncateToYear, truncateDateToYear, truncateToMinutes
    , withLocalTimeAsUtc
    , encodeXsdDateTimeAsText, decodeTextToXsdDateTime
    , encodeXsdDateAsText, decodeTextToXsdDate, encodeXsdDateAsIsoText
    , formatXsdDateTimeForHumanShortOnlyDate, formatXsdDateTimeForHumanShortOnlyTime
    , distantFuture, getXsdNamedDayOfWeek
    , xsdDateTimeToNumberHp, numberHpToXsdDateTime
    , ageAt, diffDays, lengthOfStay
    , htf_thisModulesTests
) where

#include "src/macros.h"

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad (liftM, liftM2)
import Control.Monad.Error ()
import Control.Monad.Identity (runIdentity)
import Data.Typeable (Typeable)
import Data.Ratio ((%))
import Data.List (isSuffixOf)
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import System.Time (ClockTime(..))
import System.Environment

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Control.DeepSeq (NFData(..))
import Data.Time.Clock ( UTCTime(..), getCurrentTime, utctDay, addUTCTime, diffUTCTime
                       , picosecondsToDiffTime)
import Data.Time.Calendar (Day(..), toGregorian)
import Data.Hashable (Hashable(..))
import Data.Time.Clock.POSIX
import Data.Time.Format (parseTime, formatTime)
import Data.Time.LocalTime
import Data.Serialize (Serialize(..))
import Data.Aeson

import Text.XML.XSD.DateTime

import System.Locale (defaultTimeLocale)
import Safe (readMay)
import Test.Framework

import qualified Data.Serialize as SER
import qualified Data.Time.Calendar as Cal
import qualified Safe
import qualified Data.List as List
import qualified Text.PrettyPrint.HughesPJ as P

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.SafeCopy
import Mgw.Util.Misc (runError, clockTimeToUTCTime, readM)
import Mgw.Util.Hash
import Mgw.Util.String (fixed)
import Mgw.I18n
import Mgw.Util.Logging
import Mgw.Util.TimeSpan (TimeSpan, asMicroseconds, days, picoseconds, minutes)
import Mgw.Util.QcPropHelper (mkReadShowProp)
import Mgw.Util.Preview (Ppr(..))
import qualified Mgw.Util.Misc
import qualified Mgw.Util.Text as T

_FULL_UTCTIME_FORMAT_ :: String
_FULL_UTCTIME_FORMAT_ = "%FT%T"

newtype XsdDateTime = XsdDateTime { unXsdDateTime :: UTCTime } deriving (Eq,Ord,Typeable)

newtype XsdDate = XsdDate { unXsdDate :: Day } deriving (Eq,Ord,Show,Read,Typeable)

type XsdString = String

instance Show XsdDateTime where
    showsPrec p dt = showsPrec p (formatFullXsdDateTime dt)

instance Read XsdDateTime where
    readsPrec p s =
        do (x, r) <- readsPrec p s
           y <- parseFullXsdDateTime x
           return (y, r)

instance Ppr XsdDateTime where
    ppr dt = P.text (formatXsdDateTime dt)

instance NFData XsdDate where
    rnf (XsdDate a) = rnf a

instance NFData XsdDateTime where
    rnf (XsdDateTime a) = rnf a

#if __GLASGOW_HASKELL__ < 704
instance NFData Day where
    rnf (ModifiedJulianDay a) = rnf a

instance NFData DiffTime where
    rnf a = rnf (toRational a)

instance NFData UTCTime where
    rnf (UTCTime a b) = rnf a `seq` rnf b
#endif

instance Hashable XsdDate where
    hashWithSalt s (XsdDate day) = hashWithSalt s (toModifiedJulianDay day)

instance Hashable XsdDateTime where
    hashWithSalt s (XsdDateTime utcTime) = hashWithSalt s (show utcTime)

instance Serialize XsdDateTime where
    put = putXsdDateTime
    get = getXsdDateTime

instance SecHash XsdDateTime where
    toSecHashRepr = toSecHashRepr . T.showText

instance FromJSON XsdDate where
    -- parseJSON :: Value -> Parser XsdDate
    parseJSON (String t) = decodeIsoTextToXsdDate t
    parseJSON x = fail ("Expected JSON String for XsdDate not " ++ show x)

instance ToJSON XsdDate where
    toJSON = String . encodeXsdDateAsIsoText

instance FromJSON XsdDateTime where
    parseJSON (String value) = do parsed <- parseXsdDateTime value
                                  return parsed
    parseJSON _ =  fail "Could not parse field as XsdDateTime."

instance ToJSON XsdDateTime where
    toJSON value = toJSON (T.pack (formatXsdDateTime value))

instance Random XsdDateTime where
    randomR (l,u) g = (addXsdDateTime l t, g')
      where (t, g') = randomR (0, u `diffXsdDateTime` l) g
    random = randomR (minDateTime, distantFuture)

putXsdDateTime :: XsdDateTime -> SER.PutM ()
putXsdDateTime (XsdDateTime (UTCTime (ModifiedJulianDay day) dt)) =
    SER.put day >> SER.put (toRational dt)

getXsdDateTime :: SER.Get XsdDateTime
getXsdDateTime = liftM XsdDateTime $
                 liftM2 UTCTime (liftM ModifiedJulianDay SER.get) (liftM fromRational SER.get)

instance Serialize XsdDate where
    put = putXsdDate
    get = getXsdDate

putXsdDate :: XsdDate -> SER.Put
putXsdDate (XsdDate day) = SER.put (show day)

getXsdDate :: SER.Get XsdDate
getXsdDate = liftM (XsdDate . safeRead) SER.get

instance Arbitrary Day where
    arbitrary = do day <- suchThat arbitrary (>= 0)
                   return (ModifiedJulianDay day)

instance Arbitrary UTCTime where
    arbitrary = do day <- arbitrary
                   millisecs <- choose (0, 24*60*60*1000000 - 1)
                   return $ UTCTime day (picosecondsToDiffTime (millisecs * 1000000))

instance Arbitrary XsdDateTime where
    arbitrary = do utc <- arbitrary
                   return $ XsdDateTime utc

instance Arbitrary XsdDate where
    arbitrary = do day <- arbitrary
                   return $ XsdDate day

encodeXsdDateTimeAsText :: XsdDateTime -> T.Text
encodeXsdDateTimeAsText = T.pack . show . unXsdDateTime

decodeTextToXsdDateTime :: Monad m => T.Text -> m XsdDateTime
decodeTextToXsdDateTime t = liftM XsdDateTime (readM (T.unpack t))

encodeXsdDateAsText :: XsdDate -> T.Text
encodeXsdDateAsText = T.pack . show . unXsdDate

decodeTextToXsdDate :: Monad m => T.Text -> m XsdDate
decodeTextToXsdDate t = liftM XsdDate (readM (T.unpack t))

encodeXsdDateAsIsoText :: XsdDate -> T.Text
encodeXsdDateAsIsoText = encodeXsdDateAsText

decodeIsoTextToXsdDate :: Monad m => T.Text -> m XsdDate
decodeIsoTextToXsdDate = decodeTextToXsdDate

formatXsdDateTime :: XsdDateTime -> String
formatXsdDateTime t = formatTime defaultTimeLocale "%FT%TZ" $ unXsdDateTime t

distantFuture :: XsdDateTime
distantFuture = XsdDateTime $ posixSecondsToUTCTime 424242424242

useSecondsResolution :: Bool
useSecondsResolution =
    let !env = unsafePerformIO getEnvironment
    in case List.lookup "DOCI_TIME_RESOLUTION" env of
         Just "seconds" -> True
         _ -> False
{-# NOINLINE useSecondsResolution #-}

dummyTimeSuffix :: String
dummyTimeSuffix =
    if useSecondsResolution then ", 00:00:00" else ", 00:00"

type DateTimeFormatSpec = String

defaultDateTimeFormatSpec :: DateTimeFormatSpec
defaultDateTimeFormatSpec = (longDateFormat ++ ", " ++ timeFormat)

shortDateTimeFormatSpec :: DateTimeFormatSpec
shortDateTimeFormatSpec = (shortDateFormat ++ ", " ++ timeFormat)

shortDateTimeFormatSpecWithSecs :: Bool -> DateTimeFormatSpec
shortDateTimeFormatSpecWithSecs secs =
    (shortDateFormat ++ ", " ++ if secs then timeFormatSeconds else timeFormat)

tableDateTimeFormatSpec :: DateTimeFormatSpec
tableDateTimeFormatSpec = "%Y-%m-%d %T"

tableDateTimeFormatSpecWithMillis :: DateTimeFormatSpec
tableDateTimeFormatSpecWithMillis = "%Y-%m-%d %T.%q"

dayMonthDateFormatSpec :: DateTimeFormatSpec
dayMonthDateFormatSpec = "%d.%m."

formatXsdDateTimeGen :: DateTimeFormatSpec -> XsdDateTime -> String
formatXsdDateTimeGen fmt (XsdDateTime utc) =
    let result =
            unsafePerformIO $
            do tz <- getTimeZone utc
               let zt = utcToZonedTime tz utc
               return (formatTime defaultTimeLocale fmt zt)
    in if dummyTimeSuffix `isSuffixOf` result
          then take (length result - length dummyTimeSuffix) result
          else result

formatXsdDateTimeForHuman :: XsdDateTime -> String
formatXsdDateTimeForHuman = formatXsdDateTimeGen defaultDateTimeFormatSpec

formatXsdDateTimeForHumanShort :: XsdDateTime -> String
formatXsdDateTimeForHumanShort = formatXsdDateTimeGen shortDateTimeFormatSpec

formatXsdDateTimeForHumanShortOnlyDate :: XsdDateTime -> String
formatXsdDateTimeForHumanShortOnlyDate = formatXsdDateTimeGen shortDateFormat

formatXsdDateTimeDayMonth :: XsdDateTime -> String
formatXsdDateTimeDayMonth = formatXsdDateTimeGen dayMonthDateFormatSpec

getXsdDayOfWeek :: XsdDateTime -> Int
getXsdDayOfWeek = read . (formatXsdDateTimeGen "%u")

getXsdNamedDayOfWeek :: XsdDateTime -> String
getXsdNamedDayOfWeek ts =
    case getXsdDayOfWeek ts of
      1 -> "Montag"
      2 -> "Dienstag"
      3 -> "Mittwoch"
      4 -> "Donnerstag"
      5 -> "Freitag"
      6 -> "Samstag"
      7 -> "Sonntag"
      _ -> "unbekannt"

getNextWorkday :: XsdDateTime -> XsdDateTime
getNextWorkday now =
    let nextDay = addXsdDateTime now (days 1)
        dow = getXsdDayOfWeek nextDay
    in if (dow < 6)
       then nextDay
       else getNextWorkday nextDay

formatXsdDateTimeForHumanShortOnlyTime :: XsdDateTime -> String
formatXsdDateTimeForHumanShortOnlyTime = formatXsdDateTimeGen timeFormat

formatXsdDateTimeForHumanShortWithSecs :: Bool -> XsdDateTime -> String
formatXsdDateTimeForHumanShortWithSecs secs t =
    formatXsdDateTimeGen (shortDateTimeFormatSpecWithSecs secs) t

formatXsdDateTimeForHumanTable :: XsdDateTime -> String
formatXsdDateTimeForHumanTable = formatXsdDateTimeGen tableDateTimeFormatSpec

formatXsdDateTimeForHumanTableMillis :: XsdDateTime -> String
formatXsdDateTimeForHumanTableMillis t =
    take 23 (formatXsdDateTimeGen tableDateTimeFormatSpecWithMillis t)

formatHighPrecisionTime :: XsdDateTime -> String
formatHighPrecisionTime (XsdDateTime utc) =
    unsafePerformIO $
    do tz <- getTimeZone utc
       let zt = utcToZonedTime tz utc
       return (take 12 (formatTime defaultTimeLocale "%T.%q" zt))

formatFullXsdDateTime :: XsdDateTime -> String
formatFullXsdDateTime (XsdDateTime (UTCTime day diffTime)) =
    let diffPicoseconds = toInteger (fromEnum diffTime)
        fracSeconds = diffPicoseconds `mod` (1000000000000 :: Integer)
        fullSeconds = fromInteger (diffPicoseconds - fracSeconds)
    in formatTime defaultTimeLocale _FULL_UTCTIME_FORMAT_ (UTCTime day (toEnum fullSeconds))
           ++ "Z" ++ (if fracSeconds > 0 then "+" ++ fixed 12 (show fracSeconds) ++ "ms" else "")

formatXsdDateTimeAsUtcInteger :: XsdDateTime -> Integer
formatXsdDateTimeAsUtcInteger t =
    safeRead (formatTime defaultTimeLocale "%Y%m%d%H%M%S" $ unXsdDateTime t)

xsdDateTimeToNumber :: (Read i, Integral i) => XsdDateTime -> i
xsdDateTimeToNumber t =
    xsdDateTimeToNumberWithFormat t "%Y%m%d%H%M%S"

xsdDateTimeToNumberWithFormat :: (Read i, Integral i) => XsdDateTime -> String -> i
xsdDateTimeToNumberWithFormat t format =
    let str = formatTime defaultTimeLocale format $ unXsdDateTime t
    in Safe.readNote ("Invalid datenumber: " ++ str) str

_NUMBERHP_FORMAT_ = "%Y%m%d%H%M%S%q"

xsdDateTimeToNumberHp :: XsdDateTime -> Integer
xsdDateTimeToNumberHp t =
    let str = take 20 (formatTime defaultTimeLocale _NUMBERHP_FORMAT_ $ unXsdDateTime t)
    in Safe.readNote ("Invalid datenumber: " ++ str) str

numberHpToXsdDateTime :: Monad m => Integer -> m XsdDateTime
numberHpToXsdDateTime i =
    case parseTime defaultTimeLocale _NUMBERHP_FORMAT_ (show i ++ "000000") of
      Nothing -> fail ("Could not parse number " ++ show i ++ " into XsdDateTime.")
      Just utc -> return (XsdDateTime utc)

xsdDateTimeToLocalTime :: XsdDateTime -> LocalTime
xsdDateTimeToLocalTime dt =
    let utc = unXsdDateTime dt
    in utcToLocalTime (unsafePerformIO (getTimeZone utc)) utc

parseXsdDateTime :: Monad m => T.Text -> m XsdDateTime
parseXsdDateTime s =
    case dateTime' s of
      Left parseError -> fail (show parseError)
      Right (DtZoned utcTime) -> return (XsdDateTime utcTime)
      Right (DtUnzoned localTime) ->
          pureWarn ("Timestring " ++ show s ++ " has no explicit timezone, assuming UTC") $
          return (XsdDateTime (localTimeToUTC utc localTime))

parseFullXsdDateTime :: Monad m => String -> m XsdDateTime
parseFullXsdDateTime s =
    let (isoDateTime, rest) = splitAt 19 s in
    case parseTime defaultTimeLocale _FULL_UTCTIME_FORMAT_ isoDateTime of
      Nothing -> fail ("Couldn't parse " ++ show isoDateTime
                       ++ " with format " ++ show _FULL_UTCTIME_FORMAT_)
      Just utc@(UTCTime day fullDiffTime) ->
          case rest of
            ['Z'] -> return (XsdDateTime utc)
            'Z' : '+' : milliSecondsString ->
                let (fracSecondsString, ms) = span (`elem` "0123456789") milliSecondsString in
                case ms of
                  "ms" ->
                      case readMay fracSecondsString of
                        Just fracSeconds ->
                            let diffTime = toEnum (fromEnum fullDiffTime + fracSeconds)
                            in return (XsdDateTime (UTCTime day diffTime))
                        Nothing -> fail ("Couldn't parse " ++ show fracSecondsString ++ " as "
                                         ++ "fractional seconds.")
                  _ -> fail ("Expected `ms' at end of string " ++ show milliSecondsString)
            _ -> fail ("Expected Z after " ++ show isoDateTime ++ " but found " ++ show rest)

unsafeParseXsdDateTime :: T.Text -> XsdDateTime
unsafeParseXsdDateTime s =
    case runError (parseXsdDateTime s) of
      Right dt -> dt
      Left msg ->
          safeError ("failed to parse `" ++ show s ++ "': " ++ msg)

parseXsdDate :: Monad m => String -> m XsdDate
parseXsdDate s =
    case parseTime defaultTimeLocale "%F" s of
      Just dt -> return (XsdDate dt)
      Nothing -> fail $ "XsdTypes.parseXsdDate failed to parse `" ++ s ++ "'."

unsafeParseXsdDate :: String -> XsdDate
unsafeParseXsdDate = runIdentity . parseXsdDate

formatXsdDate :: XsdDate -> String
formatXsdDate (XsdDate dt) = formatTime defaultTimeLocale "%F" dt

formatXsdDateForHuman :: XsdDate -> String
formatXsdDateForHuman (XsdDate dt) = formatTime defaultTimeLocale longDateFormat dt

formatXsdDateAsShort :: XsdDate -> String
formatXsdDateAsShort (XsdDate dt) = formatTime defaultTimeLocale longDateFormat dt

getCurrentTime' :: IO UTCTime
getCurrentTime' =
    do UTCTime !a !b <- getCurrentTime
       return $! UTCTime a b

getCurrentXsdDateTime :: IO XsdDateTime
getCurrentXsdDateTime = liftM XsdDateTime getCurrentTime'

getCurrentXsdDate :: IO XsdDate
getCurrentXsdDate = liftM (XsdDate . utctDay) getCurrentTime'

xsdDateTimeToUTCTime :: XsdDateTime -> UTCTime
xsdDateTimeToUTCTime = unXsdDateTime

utcTimeToXsdDateTime :: UTCTime -> XsdDateTime
utcTimeToXsdDateTime = XsdDateTime

xsdDateTimeToSecondsSinceEpoch :: XsdDateTime -> Integer
xsdDateTimeToSecondsSinceEpoch (XsdDateTime utcTime) =
    floor $ utcTimeToPOSIXSeconds utcTime

secondsSinceEpochToXsdDateTime :: Integer -> XsdDateTime
secondsSinceEpochToXsdDateTime i =
    XsdDateTime $ posixSecondsToUTCTime $ fromInteger i

posixSeconds1970To2001 :: POSIXTime
posixSeconds1970To2001 =
    t1 `diffUTCTime` t0
    where
      t1 = f "2001-01-01"
      t0 = f "1970-01-01"
      f s = let (Just (XsdDate t)) = parseXsdDate s in (UTCTime t 0)

xsdDateTimeToSecondsSince2001 :: XsdDateTime -> Double
xsdDateTimeToSecondsSince2001 (XsdDateTime utcTime) =
    fromRational $ toRational $ utcTimeToPOSIXSeconds utcTime - posixSeconds1970To2001

secondsSince2001ToXsdDateTime :: Double -> XsdDateTime
secondsSince2001ToXsdDateTime secs =
    XsdDateTime $ posixSecondsToUTCTime (fromRational (toRational secs) + posixSeconds1970To2001)

addXsdDateTime :: XsdDateTime -> TimeSpan -> XsdDateTime
addXsdDateTime (XsdDateTime utcTime) d =
    XsdDateTime (fromRational (asMicroseconds d % (1000 * 1000)) `addUTCTime` utcTime)

subXsdDateTime :: XsdDateTime -> TimeSpan -> XsdDateTime
subXsdDateTime (XsdDateTime utcTime) d =
    XsdDateTime (fromRational (asMicroseconds d % (-1000 * 1000)) `addUTCTime` utcTime)

diffXsdDateTime :: XsdDateTime -> XsdDateTime -> TimeSpan
diffXsdDateTime (XsdDateTime t1) (XsdDateTime t2) =
    picoseconds (round (toRational (t1 `diffUTCTime` t2) * 10^12))

clockTimeToXsdDateTime :: ClockTime -> XsdDateTime
clockTimeToXsdDateTime = XsdDateTime . clockTimeToUTCTime

xsdDateTimeToDate :: XsdDateTime -> XsdDate
xsdDateTimeToDate dt =
    case xsdDateTimeToLocalTime dt of
      (LocalTime day _) -> XsdDate day

xsdDateToDateTime :: XsdDate -> XsdDateTime
xsdDateToDateTime (XsdDate day) = XsdDateTime (UTCTime day 0)

addXsdDate :: XsdDate -> Integer -> XsdDate
addXsdDate (XsdDate (ModifiedJulianDay day)) i = XsdDate (ModifiedJulianDay (day + i))

minDate :: XsdDate
minDate = let (Just t) = parseXsdDate "1980-01-01" in t

minDateTime :: XsdDateTime
minDateTime = let (Just t) = parseXsdDateTime "1980-01-01T00:00:00Z" in t

withLocalTimeAsUtc :: (XsdDateTime -> XsdDateTime) -> XsdDateTime -> XsdDateTime
withLocalTimeAsUtc f t@(XsdDateTime utcTime) =
    let tz = unsafePerformIO (getTimeZone utcTime)
        diff = minutes (timeZoneMinutes tz)
    in f (t `addXsdDateTime` diff) `subXsdDateTime` diff

truncateToSecond :: XsdDateTime -> XsdDateTime
truncateToSecond t = unsafeParseXsdDateTime (T.pack $ formatXsdDateTime t)

truncateToMinute :: XsdDateTime -> XsdDateTime
truncateToMinute t = unsafeParseXsdDateTime (T.pack $ f (formatXsdDateTime t))
    where
      f x = take 17 x ++ "00Z"

truncateToMinutes :: Int -> XsdDateTime -> XsdDateTime
truncateToMinutes n t = unsafeParseXsdDateTime (T.pack (f (formatXsdDateTime t)))
    where
      f x =
          let (prefix, safeRead . take 2 -> minute) = splitAt 14 x in
          prefix ++ fixed 2 (show ((minute `div` n) * n)) ++ ":00Z"

truncateToHour :: XsdDateTime -> XsdDateTime
truncateToHour t = unsafeParseXsdDateTime (T.pack $ f (formatXsdDateTime t))
    where
      f x = take 14 x ++ "00:00Z"

truncateToDay :: XsdDateTime -> XsdDateTime
truncateToDay t = unsafeParseXsdDateTime (T.pack $ f (formatXsdDateTime t))
    where
      f x = take 11 x ++ "00:00:00Z"

truncateTo12h :: XsdDateTime -> XsdDateTime
truncateTo12h t = unsafeParseXsdDateTime (T.pack $ f (formatXsdDateTime t))
    where
      f x =
          let (date, rest) = splitAt 11 x
          in date ++ if take 2 rest < "12" then "00:00:00Z" else "12:00:00Z"

truncateTo6h :: XsdDateTime -> XsdDateTime
truncateTo6h t = unsafeParseXsdDateTime (T.pack $ f (formatXsdDateTime t))
    where
      f x =
          let (date, take 2 -> hour) = splitAt 11 x in
          date ++
               case () of
                 () | hour < "06" -> "00:00:00Z"
                    | hour < "12" -> "06:00:00Z"
                    | hour < "18" -> "12:00:00Z"
                    | otherwise -> "18:00:00Z"

truncateToMonth :: XsdDateTime -> XsdDateTime
truncateToMonth t = unsafeParseXsdDateTime (T.pack $ f (formatXsdDateTime t))
    where
      f x = take 8 x ++ "01T00:00:00Z"

truncateToYear :: XsdDateTime -> XsdDateTime
truncateToYear t = unsafeParseXsdDateTime (T.pack (f (formatXsdDateTime t)))
    where
      f x = take 4 x ++ "-01-01T00:00:00Z"

truncateDateToYear :: XsdDate -> XsdDate
truncateDateToYear t = unsafeParseXsdDate (f (formatXsdDate t))
    where
      f x = take 4 x ++ "-01-01"

ageAt :: XsdDate -> XsdDate -> Integer
ageAt (XsdDate ref) (XsdDate birth) =
    let (refYears, refMonth, refDay) = toGregorian ref
        (birthYears, birthMonth, birthDay) = toGregorian birth
        diff = refYears - birthYears
    in if (refMonth, refDay) < (birthMonth, birthDay)
          then diff - 1
          else diff

diffDays :: XsdDate -> XsdDate -> Integer
diffDays (XsdDate a) (XsdDate b) = Cal.diffDays a b

lengthOfStay:: XsdDate -> XsdDate -> Maybe Int
lengthOfStay admission discharge = normalize $ diffDays discharge admission
  where
    normalize x | x < 0 = Nothing
                | x == 0 = Just 1
                | otherwise = Just $ fromIntegral x

test_truncateToDay =
    do assertEqual e1 (truncateToDay d1)
       assertEqual e1 (truncateToDay d2)
       assertEqual e1 (truncateToDay d3)
    where
      e1 = (dt "2013-04-17T00:00:00Z")

      d1 = (dt "2013-04-17T01:00:00Z")
      d2 = (dt "2013-04-17T12:00:00Z")
      d3 = (dt "2013-04-17T23:00:00Z")
      dt = unsafeParseXsdDateTime

test_withLocalTimeAsUtc =
    do assertEqual (t "00:00:00+00:00") (truncateTo12h (t "13:00:00+02:00"))
       assertEqual (t "12:00:00+02:00") (withLocalTimeAsUtc truncateTo12h (t "13:00:00+02:00"))
    where
      t s = dt $ T.pack ("1984-05-22T" ++ s)
      dt = unsafeParseXsdDateTime

test_encodeXsdDateAsIsoText =
    assertEqual "2012-02-29" (encodeXsdDateAsIsoText (unsafeParseXsdDate "2012-02-29"))

prop_readShow :: XsdDateTime -> Property
prop_readShow = mkReadShowProp

test_age =
    do assertEqual 34 (ageAt (d "2013-09-30") (d "1979-09-11"))
       assertEqual 33 (ageAt (d "2013-08-30") (d "1979-09-11"))
    where
      d = unsafeParseXsdDate

test_dateTimeToDate =
    do assertEqual (d "2013-12-13") (xsdDateTimeToDate (dt "2013-12-12T23:00:00Z"))
       assertEqual (d "2013-12-12") (xsdDateTimeToDate (dt "2013-12-12T22:59:59Z"))
       assertEqual (d "2013-12-13") (xsdDateTimeToDate (dt "2013-12-13T00:00:00+01:00"))
       assertEqual (d "2013-12-12") (xsdDateTimeToDate (dt "2013-12-12T23:59:59+01:00"))
       assertEqual (d "2013-07-13") (xsdDateTimeToDate (dt "2013-07-12T22:00:00Z"))
       assertEqual (d "2013-07-12") (xsdDateTimeToDate (dt "2013-07-12T21:59:59Z"))
    where
      d = unsafeParseXsdDate
      dt = unsafeParseXsdDateTime

deriveSafeCopy 1 'base ''XsdDate
deriveSafeCopy 1 'base ''XsdDateTime
