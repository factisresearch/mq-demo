module BuildConfig where

import Types

allWays :: [String]
allWays = ["", "f", "p"]

myBuildCfg :: BuildCfg
myBuildCfg =
    BuildCfg
    { bc_ghc = "scripts/my-ghc"
    , bc_outDir = "build"
    , bc_srcDir = "src"
    , bc_libSrcDir = "lib"
    , bc_genOutDirHs = "gen-hs"
    , bc_genOutDirO = "gen-o"
    , bc_progSrcDir = "progs"
    , bc_protoDir = "../protocols/protos"
    , bc_gencodeScript = "scripts/gencode.sh"
    , bc_allWays = allWays
    , bc_flagFile = "src/build/BuildFlags.hs"
    , bc_packageFile = ".packages"
    , bc_generatedHsModulePrefixes = ["Com.Factisresearch.Checkpad.Protos"]
    }
