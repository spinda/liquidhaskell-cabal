-- | Please see the
-- <https://github.com/spinda/liquidhaskell-cabal/blob/0.2.1.0/README.md README>
-- for setup and usage instructions.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module LiquidHaskell.Cabal
  ( -- defaults
    liquidHaskellMain
  , liquidHaskellMainHooks
  , liquidHaskellHooks

    -- transformers
  , simpleUserHooksLH
  , enableLiquid
  , runLiquidPostBuild
  , runLiquidPostTest

    -- raw hooks
  , liquidHaskellPostBuildHook
  , liquidHaskellPostTestHook
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe

import Distribution.Types.UnqualComponentName
import Distribution.ModuleName hiding (main)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.ParseUtils
import Distribution.Simple
import Distribution.Simple.GHC
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup as DS
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Utils.NubList

import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath

import Debug.Trace

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
-- > main = liquidHaskellMainHooks
--
-- This is equivalent to:
--
-- > import Distribution.Simple
-- > import LiquidHaskell.Cabal
-- > main = defaultMainWithHooks $
-- >   simpleUserHooks { postBuild = liquidHaskellPostBuildHook }

liquidHaskellMainHooks :: IO ()
liquidHaskellMainHooks = defaultMainWithHooks $
  simpleUserHooks { postBuild = liquidHaskellPostBuildHook }



liquidHaskellHooks :: UserHooks
liquidHaskellHooks = runLiquidPostBuild simpleUserHooksLH

simpleUserHooksLH :: UserHooks
simpleUserHooksLH = enableLiquid simpleUserHooks

enableLiquid :: UserHooks -> UserHooks
enableLiquid hooks = hooks { hookedPrograms = liquidProgram : hookedPrograms hooks }

runLiquidPostBuild :: UserHooks -> UserHooks
runLiquidPostBuild hooks = hooks { postBuild = liquidHaskellPostBuildHook
                                 , buildHook = quietWhenNoCode (buildHook hooks)
                                 }

runLiquidPostTest :: UserHooks -> UserHooks
runLiquidPostTest  hooks = hooks { postTest = liquidHaskellPostTestHook
                                 }

liquidHaskellPostBuildHook :: Args -> BuildFlags-> PackageDescription -> LocalBuildInfo -> IO ()
liquidHaskellPostBuildHook args flags pd lbi = liquidHaskellHook args (buildVerbosity flags) pd lbi

liquidHaskellPostTestHook :: Args -> TestFlags-> PackageDescription -> LocalBuildInfo -> IO ()
liquidHaskellPostTestHook args flags pd lbi = liquidHaskellHook args (testVerbosity flags) pd lbi

-- | The raw build hook, checking the @liquidhaskell@ flag and executing the
-- LiquidHaskell binary with appropriate arguments when enabled. Can be hooked
-- into a 'UserHooks' map or invoked from within your own custom 'postBuild'
-- hook.
--
-- > import Distribution.Simple
-- > import LiquidHaskell.Cabal
-- > main = defaultMainWithHooks $
-- >   simpleUserHooks { postBuild = liquidHaskellPostBuildHook }
liquidHaskellHook :: Args -> DS.Flag Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
liquidHaskellHook args verbosityFlag pkg lbi = do
  enabled <- isFlagEnabled "liquidhaskell" lbi
  when enabled $ do
    let verbosity = fromFlag verbosityFlag
    withAllComponentsInBuildOrder pkg lbi $ \component clbi ->
      case component of
        CLib lib -> do
          let desc = "library"
          let buildInfo' = libBuildInfo lib
          sourceFilter <- makeSourceFilter desc buildInfo'
          srcs <- filterSources desc sourceFilter =<< findLibSources lib
          verifyComponent verbosity lbi clbi buildInfo' desc srcs

        CExe exe -> do
          let desc = "executable " ++ unUnqualComponentName (exeName exe)
          let buildInfo' = buildInfo exe
          sourceFilter <- makeSourceFilter desc buildInfo'
          srcs <- filterSources desc sourceFilter =<< findExeSources exe
          verifyComponent verbosity lbi clbi buildInfo' desc srcs
        _ -> return ()


liquidHaskellOptionsField :: String
liquidHaskellOptionsField = "x-liquidhaskell-options"

liquidHaskellVerifyField :: String
liquidHaskellVerifyField = "x-liquidhaskell-verify"

--------------------------------------------------------------------------------
-- Build Process Tweaks --------------------------------------------------------
--------------------------------------------------------------------------------

type CabalBuildHook = PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()

quietWhenNoCode :: CabalBuildHook -> CabalBuildHook
quietWhenNoCode hook pd lbi uh bf = do
    hook pd lbi uh bf `catch` continueWhenNoCode
  where
    noCode = any (== "-fno-code") (concatMap snd (buildProgramArgs bf))
    continueWhenNoCode
      | noCode    = \(e :: SomeException) -> return ()
      | otherwise = throw

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
        , ("--c-files="    ++) <$> cSources bi
        , userArgs
        , sources
        ]
  liquid <- requireLiquidProgram verbosity $ withPrograms lbi
  runProgram verbosity liquid args

getUserArgs :: String -> BuildInfo -> IO [ProgArg]
getUserArgs desc =
  (concat <$>) . mapM (getUserArgs' desc) . getAllCustomFieldValues liquidHaskellOptionsField

getUserArgs' :: String -> String -> IO [ProgArg]
getUserArgs' desc cmd = case parseCommandArgs cmd of
  Right args -> return args
  Left err -> dieNoVerbosity $
    "failed to parse LiquidHaskell options for " ++ desc ++ ": " ++ err

--------------------------------------------------------------------------------
-- Filter Input Sources --------------------------------------------------------
--------------------------------------------------------------------------------

type SourcePattern = Either FilePattern DirectoryPattern

data FilePattern = FilePattern
  { filePatternSource   :: !FilePath
  , filePatternCompiled :: !FilePath
  }

data DirectoryPattern = DirectoryPattern
  { directoryPatternSource   :: !FilePath
  , directoryPatternCompiled :: ![FilePath]
  }

data SourceFilter =
    All
  | Whitelist [FilePattern] [DirectoryPattern]

makeSourceFilter :: String -> BuildInfo -> IO SourceFilter
makeSourceFilter desc bi
  | null paths = return All
  | otherwise = uncurry Whitelist . partitionEithers <$> mapM (makeSourcePattern desc) paths
  where
    paths = getAllCustomFieldValues liquidHaskellVerifyField bi

makeSourcePattern :: String -> FilePath -> IO SourcePattern
makeSourcePattern desc = tryFilePattern
  where
    tryFilePattern path = do
      fileExists <- doesFileExist path
      if fileExists
         then Left . FilePattern path <$> canonicalizePath path
         else tryDirectoryPattern path
    tryDirectoryPattern path = do
      directoryExists <- doesDirectoryExist path
      if directoryExists
         then Right . DirectoryPattern path . splitDirectories <$> canonicalizePath path
         else dieWithError path
    dieWithError path = dieNoVerbosity $
      "Path passed to " ++ liquidHaskellVerifyField ++
      " for " ++ desc ++
      " does not exist: " ++ path

filterSources :: String -> SourceFilter -> [FilePath] -> IO [FilePath]
filterSources _ All paths = return paths
filterSources desc (Whitelist filePatterns directoryPatterns) paths = do
  results <- catMaybes <$> mapM (matchSourcePath filePatterns directoryPatterns) paths

  let consumedPatterns = snd <$> results
  let (consumedFilePatterns, consumedDirectoryPatterns) = partitionEithers consumedPatterns

  let unconsumedFilePaths = (\\) (filePatternSource <$> filePatterns)
                                 (filePatternSource <$> consumedFilePatterns)
  let unconsumedDirectoryPaths = (\\) (directoryPatternSource <$> directoryPatterns)
                                      (directoryPatternSource <$> consumedDirectoryPatterns)
  let unconsumedPaths = unconsumedFilePaths ++ unconsumedDirectoryPaths

  unless (null unconsumedPaths) $ dieNoVerbosity $
    "Paths passed to " ++ liquidHaskellVerifyField ++
    " for " ++ desc ++
    " do not match any source files in the component:\n" ++
    unlines (("- " ++) <$> unconsumedPaths)

  return $ fst <$> results

matchSourcePath
  :: [FilePattern]
  -> [DirectoryPattern]
  -> FilePath
  -> IO (Maybe (FilePath, SourcePattern))
matchSourcePath filePatterns directoryPatterns path = do
  path' <- canonicalizePath path
  return $ (path, ) <$> (tryFilePatterns path' <|> tryDirectoryPatterns path')
  where
    tryFilePatterns path =
      Left <$> find ((== path) . filePatternCompiled) filePatterns
    tryDirectoryPatterns path =
      let pathPieces = splitDirectories path
      in  Right <$> find ((`isPrefixOf` pathPieces) . directoryPatternCompiled) directoryPatterns

--------------------------------------------------------------------------------
-- Construct GHC Options -------------------------------------------------------
--------------------------------------------------------------------------------

makeGhcFlags
  :: Verbosity
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> BuildInfo
  -> [String]
makeGhcFlags verbosity lbi clbi bi =
#if MIN_VERSION_Cabal(1,24,0)
  renderGhcOptions (compiler lbi) (hostPlatform lbi)
#else
  renderGhcOptions (compiler lbi)
#endif
  $ sanitizeGhcOptions
  $ componentGhcOptions verbosity lbi bi clbi (buildDir lbi)

-- Mute options that interfere with Liquid Haskell
sanitizeGhcOptions :: GhcOptions -> GhcOptions
sanitizeGhcOptions opts =
  opts { ghcOptNoLink             = NoFlag -- LH uses LinkInMemory
       , ghcOptOptimisation       = NoFlag -- conflicts with interactive mode GHC
       , ghcOptProfilingMode      = NoFlag -- LH sets its own profiling mode
#if MIN_VERSION_Cabal(1,20,0)
       , ghcOptNumJobs            = NoFlag -- not relevant for LH
#endif
#if MIN_VERSION_Cabal(1,22,0)
       , ghcOptHPCDir             = NoFlag -- not relevant for LH
#endif
       , ghcOptGHCiScripts        = mempty -- may interfere with interactive mode?
       , ghcOptExtra              = filter (not . isPrefixOf "-O") (ghcOptExtra opts)
       }

--------------------------------------------------------------------------------
-- Find Component Haskell Sources ----------------------------------------------
--------------------------------------------------------------------------------

findLibSources :: Library -> IO [FilePath]
findLibSources lib = findModuleSources (libBuildInfo lib) (exposedModules lib)

findExeSources :: Executable -> IO [FilePath]
findExeSources exe = do
  moduleSrcs <- findModuleSources (buildInfo exe) []
  mainSrc    <- findFile (hsSourceDirs (buildInfo exe)) (modulePath exe)
  return (mainSrc : moduleSrcs)

findModuleSources :: BuildInfo -> [ModuleName] -> IO [FilePath]
findModuleSources bi exposed = do
  let modules = exposed ++ otherModules bi
  hsSources     <- mapM (findModuleSource ["hs", "lhs"] bi)           modules
  hsBootSources <- mapM (findModuleSource ["hs-boot", "lhs-boot"] bi) modules
  return $ catMaybes (hsSources ++ hsBootSources)

findModuleSource :: [String] -> BuildInfo -> ModuleName -> IO (Maybe FilePath)
findModuleSource suffixes bi mod =
  findFileWithExtension suffixes (hsSourceDirs bi) (toFilePath mod)

--------------------------------------------------------------------------------
-- Locating the LiquidHaskell Binary -------------------------------------------
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
  Nothing      -> getDefaultFlagValue name lbi False

getOverriddenFlagValue :: String -> LocalBuildInfo -> Maybe Bool
getOverriddenFlagValue name lbi = lookupFlagAssignment (mkFlagName name) overriddenFlags
  where
    overriddenFlags = configConfigurationsFlags (configFlags lbi)

getDefaultFlagValue :: String -> LocalBuildInfo -> Bool -> IO Bool
getDefaultFlagValue name lbi def = case pkgDescrFile lbi of
  Nothing        -> return def
  Just cabalFile -> do
    descr <- readGenericPackageDescription silent cabalFile
    let flag = find ((mkFlagName name ==) . flagName) $ genPackageFlags descr
    return $ maybe def flagDefault flag

--------------------------------------------------------------------------------
-- Cabal Field Handling --------------------------------------------------------
--------------------------------------------------------------------------------

getAllCustomFieldValues :: String -> BuildInfo -> [String]
getAllCustomFieldValues field =
  map snd . filter ((== field) . fst) . customFieldsBI

--------------------------------------------------------------------------------
-- Splitting Command Line Arguments --------------------------------------------
--------------------------------------------------------------------------------

parseCommandArgs :: String -> Either String [ProgArg]
parseCommandArgs cmd = case fieldSet field 0 cmd [] of
    ParseOk _   out -> Right $ foldMap snd out
    ParseFailed err -> Left $ snd $ locatedErrorMsg err
  where
    field = optsField liquidHaskellOptionsField
                      (OtherCompiler "LiquidHaskell")
                      id        -- get :: opts -> opts
                      (++)      -- set :: opts -> opts -> opts
                                -- where opts=[(CompilerFlavor,[String])]
