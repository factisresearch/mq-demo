{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module BuildFlags (

    BuildFlagsKey(..), BuildFlags(..), WayBuildFlags(..), getBuildFlags

) where

import Types
import Utils

import Development.Shake

import Data.Typeable
import Data.Hashable
import Data.Binary
import Data.Map (Map)
import qualified Data.Map as Map

import Control.DeepSeq

import GHC.Generics (Generic)

-- triggers rebuild if changes
newtype BuildFlagsKey = BuildFlagsKey () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

newtype BuildFlags = BuildFlags { unBuildFlags :: Map Way WayBuildFlags }
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

data WayBuildFlags
    = WayBuildFlags
      { wbf_ghcCompileFlags :: [String]
      , wbf_ghcLinkFlags :: [String]
      }
    deriving (Show, Typeable, Eq, Generic)

instance Hashable WayBuildFlags

instance NFData WayBuildFlags where
    rnf wbf = wbf_ghcCompileFlags wbf `seq` wbf_ghcLinkFlags wbf `seq` ()

instance Binary WayBuildFlags where
    put wbf =
        do put (wbf_ghcCompileFlags wbf)
           put (wbf_ghcLinkFlags wbf)
    get =
        do cFlags <- get
           lFlags <- get
           return $ WayBuildFlags cFlags lFlags

getBuildFlags :: Action BuildFlags
getBuildFlags =
    return $ BuildFlags $ Map.fromList flags
    where
      flags = [("", WayBuildFlags regularCompileFlags regularLinkFlags)
              ,("f", WayBuildFlags fastCompileFlags fastLinkFlags)
              ,("p", WayBuildFlags profCompileFlags profLinkFlags)]

-- NOTE: Define *all* build flags in this file, otherwise dependency tracking for build
-- flags doesn't work any more.
commonFlags =
    ["-Werror", "-W", "-fwarn-unused-imports", "-fwarn-unused-binds"
    ,"-fwarn-unused-matches", "-fwarn-unused-do-bind", "-fwarn-wrong-do-bind"
    ,"-pgmPcpphs", "-optP--cpp", "-rtsopts", "-threaded", "-funbox-strict-fields"]

regularCompileFlags = commonFlags ++ ["-O2"]
regularLinkFlags = ["-threaded", "-rtsopts"]

fastCompileFlags = commonFlags ++ ["-O0"]
fastLinkFlags = ["-threaded", "-rtsopts"]

profCompileFlags = commonFlags ++ ["-optc", "-O1", "-auto-all" ,"-caf-all", "-prof"]
profLinkFlags = ["-prof", "-threaded", "-rtsopts"]
