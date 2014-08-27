module Types where

type Way = String
type Alias = (String        -- name
             ,Maybe String  -- documentation
             ,[String])     -- expansion
type ModuleName = String
type PackageName = String
type ProgName = String

-- not under dependency management
data BuildCfg
    = BuildCfg
      { bc_ghc :: FilePath
      , bc_java :: String
      , bc_javac :: String
      , bc_jar :: String
      , bc_outDir :: FilePath
      , bc_srcDir :: FilePath          -- does not contain src files
      , bc_genOutDirHs :: String       -- relative to bc_outDir
      , bc_genOutDirO :: String        -- relative to bc_outDir
      , bc_genOutDirJs :: String       -- relative to bc_outDir
      , bc_libSrcDir :: String         -- relative to bc_srcDir
      , bc_progSrcDir :: String        -- relative to bc_srcDir
      , bc_protoDir :: FilePath
      , bc_serverProtoDir :: FilePath
      , bc_cpxProtoDir :: FilePath
      , bc_trinityTemplateDir :: FilePath
      , bc_trinityLibDir :: FilePath
      , bc_webifJsAll :: FilePath       -- relative to bc_outDir
      , bc_webifJsDeps :: FilePath      -- relative to bc_outDir
      , bc_soySrcDir :: FilePath
      , bc_webifJsOutDir :: FilePath    -- relative to bc_outDir
      , bc_soyPluginsSrcDir :: FilePath -- relative to bc_srcDir
      , bc_soyPluginsOutJar :: FilePath -- relative to bc_outDir
      , bc_webifJsDirs :: [FilePath]
      , bc_jsDirs :: [FilePath]
      , bc_allWays :: [Way]
      , bc_flagFile :: FilePath
      , bc_packageFile :: FilePath
      , bc_sandboxFile :: FilePath
      , bc_trinity :: FilePath
      , bc_gencodeScript :: FilePath
      , bc_generatedHsModulePrefixes :: [String]
      , bc_gjsEnvGen :: FilePath
      , bc_jsMacroDef :: FilePath
      , bc_jsPP :: FilePath
      , bc_cpmPanelUi :: FilePath
      , bc_cpmPanelUiCompiled :: FilePath -- relative to bc_outDir
      }
