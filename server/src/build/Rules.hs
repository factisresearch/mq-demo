{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Rules where

import Types
import Logging
import PkgMap
import BuildFlags
import Imports
import Utils

import Development.Shake
import Development.Shake.FilePath

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe

import Control.Monad

import qualified System.Directory as Dir

{-
Requirement 1: If an IO action makes use of some IO state, then
the rule must depend on that IO state.

Requirement: 2 If an IO action makes use of some IO state that is
modified by the build system, then the rule must depend on that IO
state before performing the IO action.

Requirement 3: After some IO state becomes a dependency it
must not change for the rest of the build run.
-}

data GhcMode = LinkMode | CompileMode
             deriving (Eq, Show)

commonProgs :: [String]
commonProgs = ["MessageQueueTests"
              ,"MessageQueueServer"
              ,"MessageQueueSendClient"
              ,"MessageQueueRecvClient"
              ]

targetAliases :: BuildCfg -> Way -> IO [Alias]
targetAliases cfg way =
    do progs <- getProgs
       return $ map (\(k,v) -> (k, Just "Build program", v)) progs ++
                  [("all", Just "Build all programs", concatMap snd progs)
                  ,("all-deps", Just "Compute dependencies for all programs",
                    map (depsFromProg . fst) progs)
                  ,("common", Just "Build common programs",
                    map prog commonProgs)
                  ]

    where
      progExtraDeps = [("WebIf", [ phonyFile cfg "webif-dev"
                                 , phonyFile cfg "webif-js-compile"
                                 ])
                      ,("ItemsToObjs", [phonyFile cfg "jsforms-check"])
                      ,("MGWTests", [phonyFile cfg "jsforms-check"
                                    ,phonyFile cfg "webif-js-check"])
                      ]
      prog name = bc_outDir cfg </> (name ++ _way)
      _way = if null way then "" else "_" ++ way
      phonyAlias name doc = (name, Just doc, [phonyFile cfg name])
      depsFromProg p = bc_outDir cfg </> bc_progSrcDir cfg </> (p ++ ".deps")
      getProgs =
          do files <- Dir.getDirectoryContents (bc_srcDir cfg </> bc_progSrcDir cfg)
             let hsFiles = filter (\f -> takeExtension f == ".hs") files
             return $ map (\f -> let b = takeBaseName f
                                     extras = fromMaybe [] (List.lookup b progExtraDeps)
                                 in (b, (bc_outDir cfg </> b ++ _way) : extras))
                          hsFiles

data BuildArgs
    = BuildArgs
      { ba_linkResource :: Resource
      , ba_fakeGhc :: Bool
      }

buildRules :: BuildCfg -> BuildArgs -> Rules ()
buildRules cfg buildArgs =
    do pkgMap <- addOracle $ \PkgMapKey{} ->
           do inSandbox <- doesFileExist (bc_sandboxFile cfg)
              Stdout dump <-
                  if inSandbox
                  then command [] "cabal" ["sandbox", "hc-pkg", "dump", "--", "--simple-output"]
                  else command [] "ghc-pkg" ["dump","--simple-output"]
              let pkgsFile = bc_packageFile cfg
              exists <- doesFileExist pkgsFile
              unless exists $ fail ("Package file " ++ pkgsFile ++
                                    " does not exist. Please run 'cabal configure'.")
              pkgs <- myReadFileLines pkgsFile
              pkgMap <- parsePkgMap dump pkgs
              -- debug ("pkgMap=" ++ show pkgMap)
              return pkgMap

       buildFlags <- addOracle $ \BuildFlagsKey{} ->
           do buildFlags <- getBuildFlags
              debug ("buildFlags=" ++ show buildFlags)
              return buildFlags

       -- transitive dependencies (packages)
       out "//*.pkgs*" *> \pkgs ->
           do dep <- myReadFileLines $ replaceExtensionKeepBoot pkgs "dep"
              let xs = map (\d -> srcToOut $ replaceExtensionKeepBoot d "pkgs")
                           (filter isHsFile dep)
              pkg <- myReadFileLines $ replaceExtension pkgs "pkg"
              ps <- fmap (Set.toList . Set.fromList . (++) pkg . concat) $
                    mapM myReadFileLines xs
              myWriteFileLines pkgs ps

       -- transitive dependencies (modules)
       out "//*.deps*" *> \deps ->
           do dep <- myReadFileLines $ replaceExtensionKeepBoot deps "dep"
              let xs = map (\d -> srcToOut $ replaceExtensionKeepBoot d "deps")
                           (filter isHsFile dep)
              ds <- fmap (Set.toList . Set.fromList . (++) dep . concat) $
                    mapM myReadFileLines xs
              myWriteFileLines deps ds

       -- direct dependencies (modules and packages)
       -- The .dep file contains the names of the dependent source files (.hs or .hs-boot)
       -- The .pkg file contains the names of the dependent packages
       let directDepends [outDep, outPkg] =
              do let (srcFile, srcMod) = outToSrc $ replaceExtensionKeepBoot outDep "hs"
                 srcContent <- myReadFile $ srcFile
                 PkgMap pMap <- pkgMap $ PkgMapKey ()
                 basePkg <-
                     case Map.lookup "Prelude" pMap of
                       Nothing -> fail ("Could not resolve module \"Prelude\". Package map: " ++
                                        show pMap)
                       Just set ->
                           case List.find (\x -> fmap pi_baseName (parsePackageId x) ==
                                                 Just "base")
                                    (Set.toList set)
                           of
                             Just base -> return base
                             Nothing -> fail "Could not found base package"
                 let (imports, usesTemplateHaskell) = hsImports srcContent
                     handleImport (deps, pkgs) imp =
                         case imp of
                           CppImport fp ->
                               do b <- doesFileExist fp
                                  return $ if b then (Set.insert fp deps, pkgs) else (deps, pkgs)
                           HsImport mod k ->
                               do let file = moduleToFile mod (extForKind "hs" k)
                                  mFp <- findFileInSrc file (isGeneratedModule mod)
                                  case mFp of
                                    Just fp -> return  (Set.insert fp deps, pkgs)
                                    Nothing ->
                                        case Map.lookup mod pMap of
                                           Just modPkgs -> return (deps, Set.union modPkgs pkgs)
                                           _ -> fail ("Could not resolve module " ++
                                                      show mod ++ " referenced from " ++
                                                      srcFile)
                 (deps', pkgs) <- foldM handleImport (Set.empty, Set.singleton basePkg) imports
                 let deps = if usesTemplateHaskell
                            then "TemplateHaskell" : Set.toList deps'
                            else Set.toList deps'
                 debug ("Generating dependency information, " ++
                        showKv ["srcFile", srcFile, "srcMod", srcMod
                               ,"basePkg", basePkg, "imports", show imports
                               ,"deps", show deps, "pkgs", show pkgs])
                 myWriteFileLinesChanged outDep deps
                 myWriteFileLinesChanged outPkg (Set.toList pkgs)
       [out "//*.dep", out "//*.pkg"] *>> directDepends
       [out "//*.dep-boot", out "//*.pkg-boot"] *>> directDepends

       -- generating code
       outGenHs "//*.hs" *> \_ -> need [phonyFile cfg "gencode"]
       outGenHs "//*.hs-boot" *> \_ -> need [phonyFile cfg "gencode"]
       let gencode mode =
             do let genScript = bc_gencodeScript cfg
                protos <- myGetDirectoryFiles (bc_protoDir cfg) ["*.proto"]
                let allProtos = protos
                need (genScript : allProtos)
                verbose <- liftIO $ isVerbose
                debugFlag <- liftIO $ isDebug
                note $ "Generating code, mode=" ++ mode ++ "..."
                mySystem INFO genScript
                         ((if debugFlag then ["--debug"]
                              else if verbose then ["--verbose"] else []) ++
                          [mode
                          ,bc_protoDir cfg
                          ,subOutDir bc_genOutDirHs
                          , "--protos"
                          ] ++ protos
                         )
       phony "gencode" $ gencode "hprotoc"

       -- calling ghc
       let ghc pkgs way mode outFiles args =
               do -- since ghc-pkg includes the ghc package, it changes if the version does
                  pkgMap $ PkgMapKey ()
                  BuildFlags flagsMap <- buildFlags $ BuildFlagsKey ()
                  let pkgFlags = "-hide-all-packages" :
                                 concatMap (\p -> ["-package-id", p]) pkgs
                      incFlags = map (\fp -> "-i" ++ fp)
                                     [subSrcDir bc_libSrcDir
                                     ,subSrcDir bc_progSrcDir
                                     ,subOutDir bc_libSrcDir
                                     ,subOutDir bc_progSrcDir
                                     ,subOutDir bc_genOutDirHs
                                     ,subOutDir bc_genOutDirO]
                      stripDot s =
                          case s of
                            ('.':xs) -> xs
                            xs -> xs
                      sufFlags = ["-hisuf", stripDot (hiExt way)
                                 ,"-osuf", stripDot (oExt way)]
                      (modeFlags, flagFun) =
                          if mode == LinkMode
                             then ([], wbf_ghcLinkFlags)
                             else (incFlags ++ sufFlags, wbf_ghcCompileFlags)
                      otherFlags = case Map.lookup way flagsMap of
                                     Just wbf -> flagFun wbf
                                     Nothing -> []
                      allFlags = (pkgFlags ++ modeFlags ++ otherFlags ++ args)
                      run = if ba_fakeGhc buildArgs
                               then mySystem INFO "touch" outFiles
                               else mySystem INFO (bc_ghc cfg) allFlags
                  if mode == LinkMode
                     then withResource (ba_linkResource buildArgs) 1 run
                     else run
       -- linking
       out ("/*") *> \prog -> do
         do let (way, progNoWay) = unWay prog
                progName = takeBaseName progNoWay
                srcProg = subSrcDir bc_progSrcDir </> (progName <.> ".hs")
                deps = srcToOut $ replaceExtension srcProg "deps"
                pkgs = srcToOut $ replaceExtension srcProg "pkgs"
            deps <- myReadFileLines deps
            let depsSet = Set.fromList deps
                hsBootOnly = filter (\d -> isHsBootFile d &&
                                           not (Set.member (stripHsBoot d) depsSet))
                                    deps
                collectHsBootOnly ext =
                    fmap concat $
                         mapM (\hs -> myReadFileLines (srcToOut $ replaceExtension hs ext))
                              hsBootOnly
            hsBootOnlyDeps <- collectHsBootOnly "deps"
            pkgs <- myReadFileLines pkgs
            hsBootOnlyPkgs <- collectHsBootOnly "pkgs"
            debug ("Rule for linking binary, " ++ showKv ["prog", show prog
                                                         ,"progName", show progName
                                                         ,"way", way
                                                         ,"srcProg", show srcProg
                                                         ,"deps", show deps
                                                         ,"pkgs", show pkgs
                                                         ,"hsBootOnly", show hsBootOnly
                                                         ,"hsBootOnlyPkgs", show hsBootOnlyPkgs
                                                         ,"hsBootOnlyDeps", show hsBootOnlyDeps])
            let os = map (\hs -> srcToOut $ replaceExtension hs (oExt way)) $
                         srcProg : (filter isHsFile (deps ++ hsBootOnlyDeps))
            need os
            let allOs = Set.toList $ Set.fromList $ os
                allPkgs = Set.toList $ Set.fromList $ pkgs ++ hsBootOnlyPkgs
            note ("Linking " ++ prog)
            ghc allPkgs way LinkMode [prog] (["-o", prog] ++ allOs)
            note ("Done linking " ++ prog)

       -- way-dependent rules
       flip mapM_ (bc_allWays cfg) $ \way ->
           do let buildHs bootSuf outFiles@(obj : _) =
                      do let depFile = replaceExtensionKeepBoot obj "dep"
                             pkgFile = replaceExtensionKeepBoot obj "pkg"
                             (hs, _) = outToSrc $ replaceExtensionKeepBoot obj "hs"
                         oDir <- objDirOf obj
                         dep <- myReadFileLines depFile
                         pkgs <- myReadFileLines pkgFile
                         debug ("Rule for compiling hs file, " ++
                                showKv ["hs", hs
                                       ,"depFile", depFile
                                       ,"pkgFile", pkgFile
                                       ,"oDir", oDir])
                         let his = map (\hs -> srcToOut $ replaceExtensionKeepBoot hs (hiExt way))
                                       (filter isHsFile dep)
                             hss = if "TemplateHaskell" `elem` dep
                                   then filter isHsFile dep
                                   else []
                             includes = filter (\x -> not (isHsFile x) && x /= "TemplateHaskell") dep
                         need $ hs : his ++ includes
                         when (isProfWay way) $ need [replaceExtension obj "o"]
                         note (hs ++ " ==> " ++ obj)
                         ghc pkgs way CompileMode outFiles ["-outputdir", oDir
                                                           ,"-c", hs
                                                           ,"-o", obj]

              -- compiling
              flip mapM_ [bc_libSrcDir cfg, bc_genOutDirO cfg] $ \d ->
                  do [outSub d ("//*" ++ oExt way), outSub d ("//*" ++ hiExt way)]
                         *>> buildHs ""
                     [outSub d ("//*" ++ oExt way ++ "-boot"),
                      outSub d ("//*" ++ hiExt way ++ "-boot")]
                         *>> buildHs "-boot"
              [outProg ("//*" ++ oExt way)] *>> buildHs ""
    where
      out x = bc_outDir cfg ++ x
      outSub d x = (bc_outDir cfg </> d) ++ x
      outProg = outSub (bc_progSrcDir cfg)
      outGenHs = outSub (bc_genOutDirHs cfg)
      outToSrc x =
          case splitDirectories x of
            p1:p2:rest | p1 == bc_outDir cfg && p2 == bc_genOutDirO cfg ->
                           (joinPath (p1 : bc_genOutDirHs cfg : rest), mkModule rest)
            p1:p2:rest | p1 == bc_outDir cfg ->
                           (joinPath (bc_srcDir cfg : p2 : rest), mkModule rest)
            parts -> (x, mkModule parts)
          where
            mkModule parts =
                case reverse parts of
                  [] -> ""
                  (last:rrest) -> List.intercalate "." $ reverse rrest ++ [dropExtension last]
      srcToOut x =
          case splitDirectories x of
            p1:p2:rest | p1 == bc_outDir cfg && p2 == bc_genOutDirHs cfg ->
                           joinPath (p1 : bc_genOutDirO cfg : rest)
            p1:rest    | p1 == bc_srcDir cfg ->
                           joinPath (bc_outDir cfg : rest)
            _ -> x
      subSrcDir f = bc_srcDir cfg </> f cfg
      subOutDir f = bc_outDir cfg </> f cfg
      objDirOf x =
          let cands = map (\d -> normalise (bc_outDir cfg </> d))
                          [bc_libSrcDir cfg, bc_progSrcDir cfg]
          in case List.find (\cand -> cand `List.isPrefixOf` x) cands of
               Nothing ->
                   let genO = bc_outDir cfg </> bc_genOutDirO cfg
                   in if normalise genO `List.isPrefixOf` x
                         then return $ genO
                         else fail ("File " ++ x ++
                                   " not contained in one of the following directories: " ++
                                   show (cands ++ [genO]))
               Just res -> return res
      findFileInSrc x isGen
          | isGen && isHsFile x = return $ Just $ bc_outDir cfg </> bc_genOutDirHs cfg </> x
          | otherwise =
              let loop [] = return Nothing
                  loop (d:ds) =
                      do let fp = d </> x
                         b <- doesFileExist fp
                         if b then return (Just fp) else loop ds
              in loop $ [subSrcDir bc_libSrcDir, subSrcDir bc_progSrcDir]
      replaceExtensionKeepBoot fname ext =
          let oldExt = takeExtension fname
              newExt = if "-boot" `List.isSuffixOf` oldExt then ext ++ "-boot" else ext
          in replaceExtension fname newExt
      moduleToFile xs ext = map (\x -> if x == '.' then '/' else x) xs <.> ext
      isHFile f = takeExtension f == ".h"
      isHsFile f = takeExtension f == ".hs" || takeExtension f == ".hs-boot"
      isHsFileNoBoot f = takeExtension f == ".hs"
      isHsBootFile f = takeExtension f == ".hs-boot"
      stripHsBoot f = if isHsBootFile f
                         then replaceExtension f ".hs"
                         else f
      mkExt e way = "." ++ way_ way ++ e
      oExt w = mkExt "o" w
      hiExt = mkExt "hi"
      isProfWay w = w == "p"
      way_ way = if null way then "" else way ++ "_"
      _way way = if null way then "" else "_" ++ way
      isGeneratedModule mod =
          any (\prefix -> prefix `List.isPrefixOf` mod) (bc_generatedHsModulePrefixes cfg)
      phony f action = (phonyFile cfg f) *> \out ->
                       do action
                          command_ [] "touch" [out]

phonyFile :: BuildCfg -> String -> FilePath
phonyFile cfg name = bc_outDir cfg </> ".phony" </> name

unWay :: String -> (String, String)
unWay s =
    case reverse s of
      way:'_':rrest -> ([way], reverse rrest)
      _ -> ("", s)
