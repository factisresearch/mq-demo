{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Mgw.Util.Misc
    ( eitherToError, errorToEither, liftError, errorToDefault, errorToMaybe, maybeToError, runError
    , runErrorTorFail, maybeToFail, eitherToFail
    , integralToHexString
    , readM, unzipF
    , readNoteVerbose
    , findNonexistingFile, findNonexistingPath, findNonexistingDirectory
    , formatTimestamp, milliSecsFromPicoSecs
    , formatTid, formatMyTid
    , anyM, findM
    , PasswordHash(..), passwordHash
    , shellQuote, shorten
    , clockTimeToUTCTime, utcTimeToClockTime
    , fromRightNote, fromLeftNote, getLeft, getRight, onLeft, onRight
    , roundInt, getDirectoryContentsTailrec, ordnum, mapMaybeSeq
    , modifyListElem
    , tailNoError
    , htf_thisModulesTests
    )
where

#include "src/macros.h"

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Applicative (Alternative, pure, empty, (<|>))

import Control.Monad (liftM)
import Control.Monad.Error (MonadError, ErrorT, runErrorT, throwError, catchError)
import Control.Monad.Identity (runIdentity)
import Control.Concurrent (ThreadId, myThreadId)
import qualified Control.Exception as Ex

import Data.Char (isPrint)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Ratio ((%))
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

import System.IO.Error
import System.FilePath
import System.Directory (doesFileExist, doesDirectoryExist)
import System.Time (ClockTime(..), CalendarTime(..), toCalendarTime)
import qualified System.Posix.Directory as Posix
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX


import qualified Data.ByteString as BS

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import qualified Crypto.Hash.SHA512 as SHA
import Test.Framework

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Encoding (encodeStringToBslUtf8)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _predM [] = return False
anyM predM (x:xs) =
    do isT <- predM x
       if isT
          then return True
          else anyM predM xs

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _pred [] = return Nothing
findM pred (x:xs) =
    do b <- pred x
       if b then return (Just x) else findM pred xs

runError :: forall a. (forall m. Monad m => m a) -> Either String a
runError x = runIdentity (runErrorT x)

eitherToError :: MonadError e m => Either e a -> m a
eitherToError resOrErr =
    case resOrErr of
      Left err -> throwError err
      Right res -> return res

errorToEither :: MonadError e m => m a -> m (Either e a)
errorToEither m = catchError (liftM Right m) (return . Left)

errorToDefault :: MonadError e m => a -> m a -> m a
errorToDefault a ma = catchError ma (\_ -> return a)

liftError :: (MonadError e m, MonadError e m1) => (forall a. m a -> m1 a) -> m a -> m1 a
liftError liftBase action = liftBase (errorToEither action) >>= eitherToError

errorToMaybe :: MonadError e m => m a -> m (Maybe a)
errorToMaybe ma = catchError (liftM Just ma) (\_ -> return Nothing)

maybeToError :: MonadError e m => String -> Maybe a -> m a
maybeToError msg ma =
    case ma of
      Nothing -> fail msg
      Just a -> return a

maybeToFail :: Monad m => String -> Maybe a -> m a
maybeToFail msg ma =
    case ma of
         Nothing -> fail msg
         Just a -> return a

eitherToFail :: Monad m => Either String a -> m a
eitherToFail e =
    case e of
      Left msg -> fail msg
      Right ok -> return ok

runErrorTorFail :: (Monad m, Show e) => ErrorT e m a -> m a
runErrorTorFail action =
    do result <- runErrorT action
       case result of
         Left err -> fail (show err)
         Right val -> return val

readM :: (Monad m, Show a, Read a) => String -> m a
readM s =
    case [x | (x,"") <- reads s] of
      (x:_) -> return x
      res -> fail  $ "Misc.readM: parse of " ++ show s ++ " returned: " ++ show res

unzipF :: (Alternative k, Alternative l, Foldable t) => t (a, b) -> (k a, l b)
unzipF = F.foldr (\(a,b) (as,bs) -> (pure a <|> as,  pure b <|> bs)) (empty,empty)


integralToHexString :: (Show a, Integral a) => a -> String
integralToHexString i
    | i == 0 = "0"
    | i > 0 = f i ""
    | otherwise = fail ("intToHexString: negative argument " ++ show i)
    where
      f :: Integral a => a -> String -> String
      f 0 acc = acc
      f i acc = f (i `div` 16) (fourBitsToChar (i `mod` 16) : acc)
      fourBitsToChar i = case i of
                           0 -> '0'
                           1 -> '1'
                           2 -> '2'
                           3 -> '3'
                           4 -> '4'
                           5 -> '5'
                           6 -> '6'
                           7 -> '7'
                           8 -> '8'
                           9 -> '9'
                           10 -> 'a'
                           11 -> 'b'
                           12 -> 'c'
                           13 -> 'd'
                           14 -> 'e'
                           15 -> 'f'
                           _ -> safeError "integralToHexString: cannot happen"

prop_integralToHexStringOk :: Int -> Bool
prop_integralToHexStringOk i' =
    let i = abs i'
    in i == safeRead ("0x" ++ integralToHexString i)

readNoteVerbose :: Read a => String -> String -> a
readNoteVerbose msg s =
    case [x | (x,t) <- reads s, ("","") <- lex t] of
      [x] -> x
      []  -> safeError $ "Prelude.read: no parse, " ++ msg ++ ", on " ++ prefix
      _   -> safeError $ "Prelude.read: ambiguous parse, " ++ msg ++ ", on " ++ prefix
    where
        prefix = '\"' : a ++ if null b then "\"" else "..."
            where (a,b) = splitAt 1024 s

findNonexistingFile :: Bool -> FilePath -> IO (Maybe FilePath)
findNonexistingFile = findNonexistingPath doesFileExist

findNonexistingDirectory :: Bool -> FilePath -> IO (Maybe FilePath)
findNonexistingDirectory = findNonexistingPath doesDirectoryExist

findNonexistingPath :: (FilePath -> IO Bool) -> Bool -> FilePath -> IO (Maybe FilePath)
findNonexistingPath test use0 fileName = loop (0::Int)
    where
      loop n =
        case () of
         _| n == 0 && not use0 ->
              do b <- test fileName
                 if not b then return $ Just fileName else loop (n + 1)
         _| n < 10000 ->
              do let (base, ext) = splitExtension fileName
                 let fileName' = printf "%s_%04d" base n <.> ext
                 b <- test fileName'
                 if not b then return $ Just fileName' else loop (n + 1)
         _| otherwise ->
              return Nothing

formatTimestamp :: Bool -> String -> ClockTime -> IO String
formatTimestamp withDate fmt ts =
    do CalendarTime year mon day hour min sec picoSec _ _ _ _ _ <- toCalendarTime ts
       let msec = milliSecsFromPicoSecs picoSec
       if withDate
         then return (printf fmt year (fromEnum mon + 1) day hour min sec msec)
         else return (printf fmt hour min sec msec)

milliSecsFromPicoSecs :: Integer -> Integer
milliSecsFromPicoSecs n = n `div` 1000000000

formatTid :: ThreadId -> String
formatTid tid = 'T' : (drop 9 $ show tid)

formatMyTid :: IO String
formatMyTid = liftM formatTid myThreadId

-- passwordHash calculated a SHA-512 hash of the UTF8 encoded password
-- and returns a hex string (as the iPad sends it)
newtype PasswordHash = PasswordHashHexSha512Utf8 { unPasswordHash :: String }
    deriving (Read, Show, Eq, Ord)

passwordHash :: String -> PasswordHash
passwordHash pw = PasswordHashHexSha512Utf8 $
                  BS.foldl (printf "%s%02x") "" $
                  SHA.hashlazy $
                  encodeStringToBslUtf8 pw


shellMetaCharacters :: String
shellMetaCharacters = " !&;`\'\"|*?~<>^()[]$\\%{}"

-- |Quotes all shell meta characters and removes non printable ones.
shellQuote :: String -> String
shellQuote "" = ""
shellQuote (x:xs) | isPrint x =
                      if x `elem` shellMetaCharacters
                         then '\\' : x : shellQuote xs
                         else x : shellQuote xs
                  | otherwise = shellQuote xs

shorten :: Int -> String -> String
shorten n s =
    let n' = length s
    in if n <= n'
          then s
          else take n s ++ " ... (" ++ show (n - n') ++ " characters dropped"

clockTimeToUTCTime :: ClockTime -> UTCTime
clockTimeToUTCTime (TOD x y) =
    let posix = fromRational $ fromInteger x + fromRational (y % 1000000000000)
    in posixSecondsToUTCTime posix

utcTimeToClockTime :: UTCTime -> ClockTime
utcTimeToClockTime utc =
    let posix = utcTimeToPOSIXSeconds utc
        rsecs = floor posix
        rpico = truncate $ abs $ 1000000000000 * (posix - (fromIntegral rsecs))
    in TOD rsecs rpico

fromRightNote :: String -> Either a b -> b
fromRightNote msg (Left _) = safeError $ "fromRight got a left value: " ++ msg
fromRightNote _ (Right x) = x

fromLeftNote :: String -> Either a b -> a
fromLeftNote msg (Right _) = safeError $ "fromLeft got a right value: " ++ msg
fromLeftNote _ (Left x) = x

getLeft :: Either a b -> Maybe a
getLeft (Left x) = Just x
getLeft _ = Nothing

getRight :: Either a b -> Maybe b
getRight (Right x) = Just x
getRight _ = Nothing

onLeft :: (a -> c) -> Either a b -> Either c b
onLeft f x =
    case x of
      Left a -> Left (f a)
      Right b -> Right b

onRight :: (b -> c) -> Either a b -> Either a c
onRight f x =
    case x of
      Left a -> Left a
      Right b -> Right (f b)

roundInt :: Int -> Int -> Int
roundInt 0 _ = safeError "roundInt: got 0 as first argument"
roundInt base i = (i `div` base) * base

prop_roundIntNeutral :: Int -> Property
prop_roundIntNeutral base =
    (base /= 0) ==> roundInt base base == base

prop_roundIntModZero :: Int -> Int -> Property
prop_roundIntModZero base i =
    (base /= 0) ==> (roundInt base i `mod` base) == 0

prop_roundIntLessEqThan :: Int -> Int -> Property
prop_roundIntLessEqThan base i =
    (base /= 0) ==> roundInt base i `op` i
    where
      op = if base < 0 then (>=) else (<=)

prop_roundIntNotTooSmall :: Int -> Int -> Property
prop_roundIntNotTooSmall base i =
    (base /= 0) ==> abs (i - roundInt base i) < abs base

getDirectoryContentsTailrec :: FilePath -> IO [FilePath]
getDirectoryContentsTailrec path =
    modifyIOError ((`ioeSetFileName` path) .
                   (`ioeSetLocation` "getDirectoryContents")) $ do
    Ex.bracket
      (Posix.openDirStream path)
      Posix.closeDirStream
      (loop [])
 where
  loop acc dirp = do
     e <- Posix.readDirStream dirp
     if null e then return (reverse acc) else do
       loop (e:acc) dirp

ordnum :: Int -> String
ordnum 1 = "first"
ordnum 2 = "second"
ordnum 3 = "third"
ordnum i
    | i < 20 = th
    | lastDigit == 1 = show i ++ "st"
    | lastDigit == 2 = show i ++ "nd"
    | lastDigit == 3 = show i ++ "rd"
    | otherwise = th
    where
      lastDigit = i `mod` 10
      th = show i ++ "th"

mapMaybeSeq :: (a -> Maybe b) -> Seq a -> Seq b
mapMaybeSeq f =
    F.foldl (\ xs x -> case f x of
                         Just y -> xs |> y
                         Nothing -> xs) Seq.empty

prop_mapMaybeSeqOk :: [Maybe Int] -> Bool
prop_mapMaybeSeqOk l =
    mapMaybe id l == F.toList (mapMaybeSeq id (Seq.fromList l))

modifyListElem :: Int -> [a] -> (a -> a) -> [a]
modifyListElem i l f =
    case splitAt i l of
      (prefix, x : suffix) -> prefix ++ (f x : suffix)
      _ -> safeError ("invalid index " ++ show i)

tailNoError :: [a] -> [a]
tailNoError [] = []
tailNoError (_:xs) = xs
