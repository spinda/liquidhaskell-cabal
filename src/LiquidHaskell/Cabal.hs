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
liquidHaskellPostBuildHook args flags pkg lbi =
  when (isFlagEnabled "liquidhaskell" lbi) $ do
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
-- Cabal Utility Functions -----------------------------------------------------
--------------------------------------------------------------------------------

isFlagEnabled :: String -> LocalBuildInfo -> Bool
isFlagEnabled name lbi = FlagName name `elem` enabledFlags
  where
    enabledFlags = map fst $ filter snd allFlags
    allFlags = configConfigurationsFlags $ configFlags lbi

parseCommandArgs :: String -> Either String [ProgArg]
parseCommandArgs cmd =
  case fieldSet field 0 cmd [] of
    ParseOk _ out -> Right $ concat $ map snd out
    ParseFailed err -> Left $ snd $ locatedErrorMsg err
  where
    field = optsField "x-liquidhaskell-options"
                      (OtherCompiler "LiquidHaskell")
                      id (++)

