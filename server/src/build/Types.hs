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
      , bc_outDir :: FilePath
      , bc_srcDir :: FilePath          -- does not contain src files
      , bc_genOutDirHs :: String       -- relative to bc_outDir
      , bc_genOutDirO :: String        -- relative to bc_outDir
      , bc_libSrcDir :: String         -- relative to bc_srcDir
      , bc_progSrcDir :: String        -- relative to bc_srcDir
      , bc_protoDir :: FilePath
      , bc_allWays :: [Way]
      , bc_flagFile :: FilePath
      , bc_packageFile :: FilePath
      , bc_gencodeScript :: FilePath
      , bc_generatedHsModulePrefixes :: [String]
      }
