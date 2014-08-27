{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module PkgMap (

    PkgMapKey(..), PkgMap(..), parsePkgMap

) where

import Types
import Utils

import Development.Shake

import qualified Distribution.InstalledPackageInfo as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Package as Cabal

import Data.Typeable
import Data.Hashable
import Data.Binary
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split
import qualified Data.List as List

import Control.DeepSeq
import Control.Monad

import GHC.Generics (Generic)

-- triggers rebuild if changes
newtype PkgMapKey = PkgMapKey () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

newtype PkgMap = PkgMap { unPkgMap :: Map ModuleName (Set PackageName) }
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- parses output of "ghc-pkg dump --simple-output"
parsePkgMap :: String -> [PackageName] -> Action PkgMap
parsePkgMap pkgDump pkgs =
    do pkgInfos <- mapM parseInfo $ splitOn "\n---\n" pkgDump
       let pkgMap = foldr (enterInfo (Set.fromList pkgs)) Map.empty pkgInfos
       return $ PkgMap pkgMap
    where
      parseInfo s =
          case Cabal.parseInstalledPackageInfo s of
            Cabal.ParseOk _ info -> return info
            Cabal.ParseFailed err ->
                fail ("Could not parse output of \"ghc-pkg dump --simple-output\": "
                      ++ show err)
      enterInfo :: Set String -> Cabal.InstalledPackageInfo -> Map ModuleName (Set PackageName)
                -> Map ModuleName (Set PackageName)
      enterInfo pkgSet info pkgMap =
          let mods = map (\name -> List.intercalate "." $ Cabal.components name)
                         (Cabal.exposedModules info)
              Cabal.InstalledPackageId pkgId = Cabal.installedPackageId info
          in if pkgId `Set.member` pkgSet
                then foldr (\mod map -> Map.insertWith Set.union mod (Set.singleton pkgId) map)
                           pkgMap mods
                else pkgMap
