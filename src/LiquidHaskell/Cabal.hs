-- | This module provides a kit to create @Setup.hs@ files with LiquidHaskell
-- integration. A sample project configured with this package
-- <https://github.com/spinda/liquidhaskell-cabal-demo is available>.
--
-- The standard, basic @Setup.hs@ configuration looks like this:
--
-- > import Distribution.Simple
-- > main = defaultMain
--
-- A simple @Setup.hs@ file that performs the standard behavior, with the
-- addition of LiquidHaskell support, can look like the following:
--
-- > import LiquidHaskell.Cabal
-- > main = liquidHaskellMain
--
-- In order for Cabal/stack to properly execute your custom @Setup.hs@ file,
-- your package's @.cabal@ file will need to set the @build-type@ to @Custom@
-- (the default is @Simple@):
--
-- > build-type: Custom
--
-- A package with LiquidHaskell integration is expected to expose a flag called
-- @liquidhaskell@, which should normally default to disabled and can be
-- implemented by adding the following stanza to your project's @.cabal@ file:
--
-- > flag liquidhaskell
-- >   description: After building, verify with LiquidHaskell
-- >   default:     False
--
-- (The spacing and description are arbitrary and open to customization.)
--
-- During the post-build phase, the hook will check whether this flag is
-- enabled and, if it is, will run the LiquidHaskell binary with the
-- appropriate command line arguments and your package's source files. This
-- assumes, of course, that the LiquidHaskell binary is installed and available
-- in @$PATH@ (please make sure you have the latest stable version installed).
--
-- Via stack, this flag can be enabled on the fly with
-- @--flag <project name>:liquidhaskell@; for example:
--
-- > stack build --flag myproject:liquidhaskell
--
-- Running @cabal-install@ directly, it will need to be enabled via @cabal
-- configure@:
--
-- > cabal configure -fliquidhaskell && cabal build
--
-- When Cabal 1.24 releases, you will be able to add a stanza like the
-- following to your @.cabal@ file to ensure @liquidhaskell-cabal@ is installed
-- and available when the @Setup.hs@ file is built:
--
-- > custom-setup
-- >   setup-depends: base, Cabal, liquidhaskell-cabal
--
-- Unfortunately Cabal <1.24 has no means of tracking build dependencies for
-- @Setup.hs@ files. If you're using Cabal directly, you'll need to install
-- @liquidhaskell-cabal@ manually before configuring/building your project:
--
-- > cabal install liquidhaskell-cabal
--
-- If you're using stack, add @liquidhaskell-cabal@ to each of your components'
-- @build-depends@, add @liquidhaskell-cabal-0.1.0.0@ to your @stack.yaml@'s
-- <https://github.com/commercialhaskell/stack/blob/master/doc/yaml_configuration.md#extra-deps extra-deps section>,
-- and add an
-- <https://github.com/commercialhaskell/stack/blob/master/doc/yaml_configuration.md#explicit-setup-deps explicit-setup-deps section> to your @stack.yaml@:
--
-- > explicit-setup-deps:
-- >   "*": true
-- > extra-deps:
-- > - liquidhaskell-cabal-0.1.0.0
--
-- Then you can build your project as you normally would with stack.
--
-- Each component of the package (libary and executables) can specify its own
-- extra command line flags to pass to LiquidHaskell (these are described in
-- the <https://github.com/ucsd-progsys/liquidhaskell LiquidHaskell README>).
-- Simply add an @x-liquidhaskell-options@ field to the relevant components:
--
-- > library
-- >   (... other fields ...)
-- >   x-liquidhaskell-options: --diff --no-termination
--
-- > executable myexecutable
-- >   (... other fields ...)
-- >   x-liquidhaskell-options: --diff
--
-- For most projects, the simple sample @Setup.hs@ file given above, using
-- 'liquidHaskellMain', should be sufficient. However, for those already using
-- custom @Setup.hs@ files, @liquidhaskell-cabal@ exposes more granular means
-- of invoking the hook. Using 'liquidHaskellHooks', the basic @Setup.hs@ file
-- is equivalent to:
--
-- > import Distribution.Simple
-- > import LiquidHaskell.Cabal
-- > main = defaultMainWithHooks liquidHaskellHooks
--
-- Using 'liquidHaskellPostBuildHook', this is further equivalent to:
--
-- > import Distribution.Simple
-- > import LiquidHaskell.Cabal
-- > main = defaultMainWithHooks $
-- >   simpleUserHooks { postBuild = liquidHaskellPostBuildHook }
--
-- Projects already using a @postBuild@ hook can invoke
-- 'liquidHaskellPostBuildHook' from within this existing hook where
-- appropriate.

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

