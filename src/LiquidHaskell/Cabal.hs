module LiquidHaskell.Cabal (
  -- * Setup.hs Hooks Kit
    liquidHaskellMain
  , liquidHaskellHooks
  , liquidHaskellPostBuildHook
  ) where

import Control.Monad

import Data.List
import Data.Maybe

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

liquidHaskellMain :: IO ()
liquidHaskellMain = defaultMainWithHooks liquidHaskellHooks

liquidHaskellHooks :: UserHooks
liquidHaskellHooks = simpleUserHooks { postBuild = liquidHaskellPostBuildHook }

liquidHaskellPostBuildHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
liquidHaskellPostBuildHook args flags pkg lbi = do
  enabled <- isFlagEnabled "liquidhaskell" lbi
  when enabled $ do
    let verbosity = fromFlag $ buildVerbosity flags
    withAllComponentsInBuildOrder pkg lbi $ \component clbi ->
      case component of
        CLib lib -> verifyComponent verbosity lbi clbi (libBuildInfo lib) "library"
                      =<< findLibSources lib
        CExe exe -> verifyComponent verbosity lbi clbi (buildInfo exe) ("executable " ++ exeName exe)
                      =<< findExeSources exe
        _ -> return ()

--------------------------------------------------------------------------------
-- Verify a Library or Executable Component ------------------------------------
--------------------------------------------------------------------------------

verifyComponent :: Verbosity -> LocalBuildInfo -> ComponentLocalBuildInfo -> BuildInfo -> String -> [FilePath] -> IO ()
verifyComponent verbosity lbi clbi bi desc sources = do
  userArgs <- getUserArgs desc bi
  let ghcFlags = renderGhcOptions (compiler lbi) $
        componentGhcOptions verbosity lbi bi clbi $ buildDir lbi
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

