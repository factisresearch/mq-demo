{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
module Mgw.Util.SafeCopy
    ( module Data.SafeCopy
    , safeEncode, safeEncode', safeDecode, safeDecode', safeDecodeLazy
    , vectorToOSMap, osMapToVector, seqToVector, vectorToSeq
    , getMap, putMap
    , getOSMap, putOSMap
    , getSeq, putSeq
    , deriveSafeCopyCtx
    )
where

#include "src/macros.h"

----------------------------------------
-- LOCAL
----------------------------------------
import Mgw.Util.Seq as Seq
import Mgw.Util.OSMap (OSMap)
import Mgw.Util.Misc (eitherToFail)
import qualified Mgw.Util.OSMap as OSMap

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.SafeCopy
import Data.Serialize
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Fusion.Stream as VS

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Monad (liftM)
import Data.Foldable as F
import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.MultiSet as MultiSet
import qualified Language.Haskell.TH as TH

safeEncode :: SafeCopy a => a -> BS.ByteString
safeEncode = runPut . safePut

safeEncode' :: SafeCopy a => a -> BSL.ByteString
safeEncode' = runPutLazy . safePut

safeDecode :: (Monad m, SafeCopy a) => BS.ByteString -> m a
safeDecode bs = eitherToFail (runGet safeGet bs)

safeDecodeLazy :: (Monad m, SafeCopy a) => BSL.ByteString -> m a
safeDecodeLazy bs = eitherToFail (runGetLazy safeGet bs)

safeDecode' :: (Monad m, SafeCopy a) => BSL.ByteString -> m a
safeDecode' bsl = eitherToFail (runGetLazy safeGet bsl)

vector_getCopy :: SafeCopy a => Contained (Get (V.Vector a))
vector_getCopy =
    contain $
    do getElem <- getSafeGet
       len <- safeGet
       V.generateM len (const getElem)
{-# INLINE vector_getCopy #-}

vector_putCopy :: SafeCopy a => V.Vector a -> Contained Put
vector_putCopy vec =
    contain $
    do putElem <- getSafePut
       safePut (V.length vec)
       V.mapM_ putElem vec
{-# INLINE vector_putCopy #-}

instance SafeCopy a => SafeCopy (V.Vector a) where
    getCopy = vector_getCopy
    putCopy = vector_putCopy

newtype OldOSMap k v = OldOSMap { unOldOSMap :: OSMap k v }

instance (Ord k, SafeCopy k, SafeCopy v) => SafeCopy (OldOSMap k v) where
    putCopy _ = safeError "Should never serialize the old OSMap way."
    getCopy = contain $ liftM (OldOSMap . OSMap.fromDataMap) safeGet

instance (Ord k, SafeCopy k, SafeCopy v) => SafeCopy (OSMap k v) where
    version  = 2
    kind = extension
    putCopy = contain . putOSMap
    getCopy = contain getOSMap

instance (Ord k, SafeCopy k, SafeCopy v) => Migrate (OSMap k v) where
    type MigrateFrom (OSMap k v) = OldOSMap k v
    migrate = unOldOSMap

osMapToVector :: OSMap k v -> V.Vector (k, v)
osMapToVector = VG.unstream . OSMap.foldrWithKey (\k v -> VS.cons (k,v)) VS.empty

vectorToOSMap :: Ord k => V.Vector (k, v) -> OSMap k v
vectorToOSMap = OSMap.fromDistinctAscList . V.toList

vectorToSeq :: V.Vector a -> Seq a
vectorToSeq = V.foldl' (|>) Seq.empty

seqToVector :: Seq a -> V.Vector a
seqToVector = VG.unstream . F.foldr VS.cons VS.empty

putSeq :: SafeCopy a => Putter (Seq a)
putSeq xs =
    do safePut (Seq.length xs)
       putElem <- getSafePut
       F.forM_ xs putElem

getSeq :: SafeCopy a => Get (Seq a)
getSeq =
    do len <- safeGet
       getElem <- getSafeGet
       let loop !i !acc
               | i == 0 = return acc
               | otherwise =
                   do next <- getElem
                      loop (i - 1) (acc |> next)
       loop (len :: Int) Seq.empty

putMap :: (SafeCopy a, SafeCopy b) => Putter (Map.Map a b)
putMap = putMapGen Map.size Map.foldrWithKey

getMap :: (Ord a, SafeCopy a, SafeCopy b) => Get (Map.Map a b)
getMap = getMapGen Map.insert Map.empty

putHashMap :: (SafeCopy a, SafeCopy b) => Putter (HashMap.HashMap a b)
putHashMap = putMapGen HashMap.size HashMap.foldrWithKey

getHashMap :: (Hashable a, Eq a, SafeCopy a, SafeCopy b) => Get (HashMap.HashMap a b)
getHashMap = getMapGen HashMap.insert HashMap.empty

instance (SafeCopy k, SafeCopy a, Hashable k, Eq k) => SafeCopy (HashMap.HashMap k a) where
    getCopy = contain getHashMap
    putCopy = contain . putHashMap

putMapGen :: (SafeCopy k, SafeCopy a)
          => (t k a -> Int)
          -> (forall b . ((k -> a -> b -> b) -> b -> t k a -> b))
          -> Putter (t k a)
putMapGen size foldrWithKey xs =
    do safePut (size xs)
       putKey <- getSafePut
       putValue <- getSafePut
       foldrWithKey (\k v rest -> putKey k >> putValue v >> rest) (return ()) xs

getMapGen :: (SafeCopy k, SafeCopy a)
          => (k -> a -> t k a -> t k a)
          -> (t k a)
          -> Get (t k a)
getMapGen insert empty =
    do len <- safeGet
       getKey <- getSafeGet
       getValue <- getSafeGet
       let loop !i !acc
               | i == 0 = return acc
               | otherwise =
                   do key <- getKey
                      val <- getValue
                      loop (i - 1) (insert key val acc)
       loop (len :: Int) empty

putOSMap :: (SafeCopy a, SafeCopy b) => Putter (OSMap.OSMap a b)
putOSMap xs =
    do safePut (OSMap.size xs)
       putKey <- getSafePut
       putValue <- getSafePut
       OSMap.foldrWithKey (\k v rest -> putKey k >> putValue v >> rest) (return ()) xs

getOSMap :: (Ord a, SafeCopy a, SafeCopy b) => Get (OSMap.OSMap a b)
getOSMap =
    do len <- safeGet
       getKey <- getSafeGet
       getValue <- getSafeGet
       let loop !i !acc
               | i == 0 = return acc
               | otherwise =
                   do key <- getKey
                      val <- getValue
                      loop (i - 1) (OSMap.insert key val acc)
       loop (len :: Int) OSMap.empty

instance (SafeCopy a, Hashable a, Eq a) => SafeCopy (HashSet.HashSet a) where
    getCopy = contain $
        do l <- safeGet
           return $ HashSet.fromList l
    putCopy hs = contain $ safePut (HashSet.toList hs)

instance (SafeCopy a, Ord a) => SafeCopy (MultiSet.MultiSet a) where
    getCopy = contain $
        do l <- safeGet
           return $ MultiSet.fromList l
    putCopy hs = contain $ safePut (MultiSet.toList hs)

deriveSafeCopyCtx :: Version a
                  -> TH.Name
                  -> TH.Name
                  -> ([TH.TypeQ] -> [TH.PredQ])
                  -> (TH.Q [TH.Dec])
deriveSafeCopyCtx version kindName tyName extraPreds =
    do decs <- deriveSafeCopy version kindName tyName
       case decs of
         [TH.InstanceD ctx ty body] ->
             do let args = reverse (collectArgs ty)
                extras <- sequence (extraPreds (map return args))
                let newCtx = ctx ++ extras
                -- _ <- fail ("args: " ++ show args ++", ty: " ++ show ty)
                return [TH.InstanceD newCtx ty body]
         _ -> error ("Unexpected declarations returned by deriveSafeCopy: " ++ show (TH.ppr decs))
    where
      collectArgs :: TH.Type -> [TH.Type]
      collectArgs ty =
          let loop ty =
                  case ty of
                    (TH.AppT l r) ->
                        case l of
                          TH.AppT _ _ -> r : loop l
                          _ -> [r]
                    _ -> []
          in case ty of
               TH.AppT _ r -> loop r
               _ -> []
