-- | Please see the
-- <https://github.com/spinda/liquidhaskell-cabal/blob/0.1.1.0/README.md README>
-- for setup and usage instructions.

{-# LANGUAGE CPP #-}

module LiquidHaskell.Cabal (
    -- * Setup.hs Hooks Kit
    liquidHaskellMain
  , liquidHaskellHooks
  , liquidHaskellPostBuildHook
  ) where

import Control.Monad

import Data.List
import Data.Maybe
import Data.Monoid

import Distribution.ModuleName hiding (main)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.ParseUtils
import Distribution.Simple
import Distribution.Simple.GHC
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity

import System.FilePath

--------------------------------------------------------------------------------
-- Setup.hs Hooks Kit ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | The simplest method of incorporating LiquidHaskell into a @Setup.hs@ file.
--
-- > import LiquidHaskell.Cabal
-- > main = liquidHaskellMain
--
-- This is equivalent to:
--
-- > import Distribution.Simple
-- > import LiquidHaskell.Cabal
-- > main = defaultMainWithHooks liquidHaskellHooks
liquidHaskellMain :: IO ()
liquidHaskellMain = defaultMainWithHooks liquidHaskellHooks

-- | Cabal's 'simpleUserHooks' configured with 'liquidHaskellPostBuildHook' in
-- the 'postBuild' field. Can be customized with your project's own user hooks.
--
-- > import Distribution.Simple
-- > import LiquidHaskell.Cabal
-- > main = defaultMainWithHooks liquidHaskellHooks
--
-- This is equivalent to:
--
-- > import Distribution.Simple
-- > import LiquidHaskell.Cabal
-- > main = defaultMainWithHooks $
-- >   simpleUserHooks { postBuild = liquidHaskellPostBuildHook }
liquidHaskellHooks :: UserHooks
liquidHaskellHooks = simpleUserHooks { postBuild = liquidHaskellPostBuildHook }

-- | The raw build hook, checking the @liquidhaskell@ flag and executing the
-- LiquidHaskell binary with appropriate arguments when enabled. Can be hooked
-- into a 'UserHooks' map or invoked from within your own custom 'postBuild'
-- hook.
--
-- > import Distribution.Simple
-- > import LiquidHaskell.Cabal
-- > main = defaultMainWithHooks $
-- >   simpleUserHooks { postBuild = liquidHaskellPostBuildHook }
liquidHaskellPostBuildHook :: Args -> BuildFlags -> PackageDescription
                           -> LocalBuildInfo -> IO ()
liquidHaskellPostBuildHook args flags pkg lbi = do
  enabled <- isFlagEnabled "liquidhaskell" lbi
  when enabled $ do
    let verbosity = fromFlag $ buildVerbosity flags
    withAllComponentsInBuildOrder pkg lbi $ \component clbi ->
      case component of
        CLib lib -> verifyComponent verbosity lbi clbi (libBuildInfo lib)
                    "library"
                      =<< findLibSources lib
        CExe exe -> verifyComponent verbosity lbi clbi (buildInfo exe)
                    ("executable " ++ exeName exe)
                      =<< findExeSources exe
        _ -> return ()

--------------------------------------------------------------------------------
-- Verify a Library or Executable Component ------------------------------------
--------------------------------------------------------------------------------

verifyComponent :: Verbosity -> LocalBuildInfo -> ComponentLocalBuildInfo
                -> BuildInfo -> String -> [FilePath] -> IO ()
verifyComponent verbosity lbi clbi bi desc sources = do
  userArgs <- getUserArgs desc bi
  let ghcFlags = makeGhcFlags verbosity lbi clbi bi
  let args = concat
        [ ("--ghc-option=" ++) <$> ghcFlags
        , ("--c-files=" ++) <$> (cSources bi)
        , userArgs
        , sources
        ]
  liquid <- requireLiquidProgram verbosity $ withPrograms lbi
  runProgram verbosity liquid args

getUserArgs :: String -> BuildInfo -> IO [ProgArg]
getUserArgs desc bi =
  case lookup "x-liquidhaskell-options" (customFieldsBI bi) of
    Nothing -> return []
    Just cmd ->
      case parseCommandArgs cmd of
        Right args -> return args
        Left err -> die $
          "failed to parse LiquidHaskell options for " ++ desc ++ ": " ++ err

--------------------------------------------------------------------------------
-- Construct GHC Options -------------------------------------------------------
--------------------------------------------------------------------------------

makeGhcFlags :: Verbosity -> LocalBuildInfo -> ComponentLocalBuildInfo
             -> BuildInfo -> [String]
makeGhcFlags verbosity lbi clbi bi =
  renderGhcOptions (compiler lbi) $
  sanitizeGhcOptions $
  componentGhcOptions verbosity lbi bi clbi $ buildDir lbi

-- Whitelist which GHC options get passed along to LiquidHaskell.
-- (see issue #2)
sanitizeGhcOptions :: GhcOptions -> GhcOptions
sanitizeGhcOptions opts = GhcOptions
  { ghcOptMode               = ghcOptMode               opts
  , ghcOptExtra              = ghcOptExtra              opts
  , ghcOptExtraDefault       = ghcOptExtraDefault       opts
  , ghcOptInputFiles         = ghcOptInputFiles         opts
  , ghcOptInputModules       = ghcOptInputModules       opts
  , ghcOptOutputFile         = ghcOptOutputFile         opts
  , ghcOptOutputDynFile      = ghcOptOutputDynFile      opts
  , ghcOptSourcePathClear    = ghcOptSourcePathClear    opts
  , ghcOptSourcePath         = ghcOptSourcePath         opts
#if MIN_VERSION_Cabal(1,22,0)
  , ghcOptPackageKey         = ghcOptPackageKey         opts
#else
  , ghcOptPackageName        = ghcOptPackageName        opts
#endif
  , ghcOptPackageDBs         = ghcOptPackageDBs         opts
  , ghcOptPackages           = ghcOptPackages           opts
  , ghcOptHideAllPackages    = ghcOptHideAllPackages    opts
  , ghcOptNoAutoLinkPackages = ghcOptNoAutoLinkPackages opts
#if MIN_VERSION_Cabal(1,22,0)
  , ghcOptSigOf              = ghcOptSigOf              opts
#endif
  , ghcOptLinkLibs           = ghcOptLinkLibs           opts
  , ghcOptLinkLibPath        = ghcOptLinkLibPath        opts
  , ghcOptLinkOptions        = ghcOptLinkOptions        opts
  , ghcOptLinkFrameworks     = ghcOptLinkFrameworks     opts
  , ghcOptNoLink             = NoFlag -- LH uses LinkInMemory
  , ghcOptLinkNoHsMain       = ghcOptLinkNoHsMain       opts
  , ghcOptCcOptions          = ghcOptCcOptions          opts
  , ghcOptCppOptions         = ghcOptCppOptions         opts
  , ghcOptCppIncludePath     = ghcOptCppIncludePath     opts
  , ghcOptCppIncludes        = ghcOptCppIncludes        opts
  , ghcOptFfiIncludes        = ghcOptFfiIncludes        opts
  , ghcOptLanguage           = ghcOptLanguage           opts
  , ghcOptExtensions         = ghcOptExtensions         opts
  , ghcOptExtensionMap       = ghcOptExtensionMap       opts
  , ghcOptOptimisation       = NoFlag -- conflicts with interactive mode GHC
#if MIN_VERSION_Cabal(1,22,0)
  , ghcOptDebugInfo          = ghcOptDebugInfo          opts
#endif
  , ghcOptProfilingMode      = NoFlag -- LH sets its own profiling mode
  , ghcOptSplitObjs          = ghcOptSplitObjs          opts
#if MIN_VERSION_Cabal(1,20,0)
  , ghcOptNumJobs            = NoFlag -- not relevant for LH
#endif
#if MIN_VERSION_Cabal(1,22,0)
  , ghcOptHPCDir             = NoFlag -- not relevant for LH
#endif
  , ghcOptGHCiScripts        = mempty -- may interfere with interactive mode?
  , ghcOptHiSuffix           = ghcOptHiSuffix           opts
  , ghcOptObjSuffix          = ghcOptObjSuffix          opts
  , ghcOptDynHiSuffix        = ghcOptDynHiSuffix        opts
  , ghcOptDynObjSuffix       = ghcOptDynObjSuffix       opts
  , ghcOptHiDir              = ghcOptHiDir              opts
  , ghcOptObjDir             = ghcOptObjDir             opts
  , ghcOptOutputDir          = ghcOptOutputDir          opts
  , ghcOptStubDir            = ghcOptStubDir            opts
  , ghcOptDynLinkMode        = ghcOptDynLinkMode        opts
  , ghcOptShared             = ghcOptShared             opts
  , ghcOptFPic               = ghcOptFPic               opts
  , ghcOptDylibName          = ghcOptDylibName          opts
#if MIN_VERSION_Cabal(1,22,0)
  , ghcOptRPaths             = ghcOptRPaths             opts
#endif
  , ghcOptVerbosity          = ghcOptVerbosity          opts
  , ghcOptCabal              = ghcOptCabal              opts
  }

--------------------------------------------------------------------------------
-- Find Component Haskell Sources ----------------------------------------------
--------------------------------------------------------------------------------

findLibSources :: Library -> IO [FilePath]
findLibSources lib = findModuleSources (libBuildInfo lib) (exposedModules lib)

findExeSources :: Executable -> IO [FilePath]
findExeSources exe = do
  moduleSrcs <- findModuleSources (buildInfo exe) []
  mainSrc <- findFile (hsSourceDirs $ buildInfo exe) (modulePath exe)
  return (mainSrc : moduleSrcs)

findModuleSources :: BuildInfo -> [ModuleName] -> IO [FilePath]
findModuleSources bi exposed = do
  let modules = exposed ++ otherModules bi
  hsSources <- mapM (findModuleSource ["hs", "lhs"] bi) modules
  hsBootSources <- mapM (findModuleSource ["hs-boot", "lhs-boot"] bi) modules
  return $ catMaybes (hsSources ++ hsBootSources)

findModuleSource :: [String] -> BuildInfo -> ModuleName -> IO (Maybe FilePath)
findModuleSource suffixes bi mod =
  findFileWithExtension suffixes (hsSourceDirs bi) (toFilePath mod)

--------------------------------------------------------------------------------
-- Located the LiquidHaskell Binary --------------------------------------------
--------------------------------------------------------------------------------

requireLiquidProgram :: Verbosity -> ProgramDb -> IO ConfiguredProgram
requireLiquidProgram verbosity db =
  fst <$> requireProgram verbosity liquidProgram db

liquidProgram :: Program
liquidProgram = simpleProgram "liquid"

--------------------------------------------------------------------------------
-- Cabal Flag Handling ---------------------------------------------------------
--------------------------------------------------------------------------------

isFlagEnabled :: String -> LocalBuildInfo -> IO Bool
isFlagEnabled name lbi = case getOverriddenFlagValue name lbi of
  Just enabled -> return enabled
  Nothing -> getDefaultFlagValue name lbi False

getOverriddenFlagValue :: String -> LocalBuildInfo -> Maybe Bool
getOverriddenFlagValue name lbi = lookup (FlagName name) overriddenFlags
  where
    overriddenFlags = configConfigurationsFlags $ configFlags lbi

getDefaultFlagValue :: String -> LocalBuildInfo -> Bool -> IO Bool
getDefaultFlagValue name lbi def = case pkgDescrFile lbi of
  Nothing -> return def
  Just cabalFile -> do
    descr <- readPackageDescription silent cabalFile
    let flag = find ((FlagName name ==) . flagName) $ genPackageFlags descr
    return $ maybe def flagDefault flag

--------------------------------------------------------------------------------
-- Splitting Command Line Arguments --------------------------------------------
--------------------------------------------------------------------------------

parseCommandArgs :: String -> Either String [ProgArg]
parseCommandArgs cmd =
  case fieldSet field 0 cmd [] of
    ParseOk _ out -> Right $ concat $ map snd out
    ParseFailed err -> Left $ snd $ locatedErrorMsg err
  where
    field = optsField "x-liquidhaskell-options"
                      (OtherCompiler "LiquidHaskell")
                      id (++)

