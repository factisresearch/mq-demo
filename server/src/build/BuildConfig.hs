module BuildConfig where

import Types

allWays :: [String]
allWays = ["", "f", "p"]

myBuildCfg :: BuildCfg
myBuildCfg =
    BuildCfg
    { bc_ghc = "scripts/my-ghc"
    , bc_java = "java"
    , bc_javac = "javac"
    , bc_jar = "jar"
    , bc_outDir = "build"
    , bc_srcDir = "src"
    , bc_libSrcDir = "lib"
    , bc_genOutDirHs = "gen-hs"
    , bc_genOutDirJs = "gen-js"
    , bc_genOutDirO = "gen-o"
    , bc_progSrcDir = "progs"
    , bc_protoDir = "../protocols/protos"
    , bc_serverProtoDir = "protos"
    , bc_cpxProtoDir = "cpxprotos"
    , bc_webifJsAll = "webif/all.js"
    , bc_webifJsDeps = "webif/deps.js"
    , bc_soySrcDir = "data/webif/templates"
    , bc_webifJsOutDir = "webif/js"
    , bc_soyPluginsSrcDir = "misc/soyplugins"
    , bc_soyPluginsOutJar = "misc/soyplugins.jar"
    , bc_webifJsDirs = ["data/webif/js"]
    , bc_jsDirs = ["data/jsforms"]
    , bc_trinity = "build/Trinity_f"
    , bc_trinityTemplateDir = "data/trinity/templates"
    , bc_trinityLibDir = "data/trinity/lib"
    , bc_gencodeScript = "scripts/gencode.sh"
    , bc_allWays = allWays
    , bc_flagFile = "src/build/BuildFlags.hs"
    , bc_packageFile = ".packages"
    , bc_sandboxFile = "cabal.sandbox.config"
    , bc_generatedHsModulePrefixes = ["Com.Factisresearch.Checkpad.Protos"
                                     ,"De.Cpx.Protobuf"
                                     ,"Mgw.Wf.Gencode"]
    , bc_gjsEnvGen = "build/MkGJsTestEnv_f"
    , bc_jsMacroDef = "data/jsforms/macros.h"
    , bc_jsPP = "build/JsPP_f"
    , bc_cpmPanelUi = "data/cpm-panel/code/ui/"
    , bc_cpmPanelUiCompiled = "cpm-panel/ui.js"
    }
