{-# LANGUAGE CPP #-}
module Mgw.Util.QcPropHelper
    ( mkReadShowProp, mkGetPutProp, mkSafeGetPutProp, showSerialization
    )
where

#include "src/macros.h"

import Data.Serialize
import Mgw.Util.SafeCopy

import Test.QuickCheck
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSLC
import qualified Data.ByteString.Base16 as Base16

data Rec a = Rec  { _rec_withLabel :: a } deriving (Eq, Read, Show)

mkReadShowProp :: (Eq a, Read a, Show a) => a -> Property
mkReadShowProp a =
    do t1 <- test a
       t2 <- test (Just a)
       t3 <- test (a,a)
       t4 <- test (Just a, Just ([a,a,a], Rec a, a))
       conjoin [t1,t2,t3,t4]
    where
      test :: (Eq a, Read a, Show a) => a -> Gen Prop
      test v =
          do p1 <- report 5 v
             p2 <- report 11 v
             conjoin [p1, p2]
      report :: (Eq a, Read a, Show a) => Int -> a -> Gen Prop
      report p v =
          let str = showsPrec p v ""
              parse = readsPrec p str
          in whenFail (putStrLn $ "Value    : " ++ str ++ "\nparsed as: " ++ show parse) $
             property (parse == [(v, "")])

mkSafeGetPutProp :: (Eq a, SafeCopy a, Show a) => a -> Property
mkSafeGetPutProp x =
    do test x
    where
      test x =
          let bs = runPut (safePut x)
              ex = runGet safeGet bs
          in whenFail (putStrLn ("Value           : " ++ show x ++ "\n" ++
                                 "serialized to   : " ++ showSerialization bs ++ "\n" ++
                                 "had parse error : " ++ show ex)) $
             property (Right x == ex)

mkGetPutProp :: (Eq a, Serialize a, Show a) => a -> Property
mkGetPutProp x =
    do test x
    where
      test x =
          let bs = runPut (put x)
              ex = runGet get bs
          in whenFail (putStrLn ("Value           : " ++ show x ++ "\n" ++
                                 "serialized to   : " ++ showSerialization bs ++ "\n" ++
                                 "had parse error : " ++ show ex)) $
             property (Right x == ex)

showSerialization :: ByteString -> String
showSerialization = BSLC.unpack . Base16.encode
