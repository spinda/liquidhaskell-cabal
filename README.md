# liquidhaskell-cabal

*[Liquid Haskell](https://github.com/ucsd-progsys/liquidhaskell/) integration for Cabal and Stack.*

[![Hackage](https://img.shields.io/hackage/v/liquidhaskell-cabal.svg)](https://hackage.haskell.org/package/liquidhaskell-cabal)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/liquidhaskell-cabal.svg)](http://packdeps.haskellers.com/feed?needle=liquidhaskell-cabal)

`liquidhaskell-cabal` provides drop-in LiquidHaskell integration for projects built with Cabal and/or Stack.

## Setting Up

*(See [`liquidhaskell-cabal-demo`](https://github.com/spinda/liquidhaskell-cabal-demo) for an example project setup.)*

1. Make sure you have LiquidHaskell version 0.6 or above installed and available in your `$PATH`.

1. Open up your `Setup.hs` file. For most projects, it will look like this:

   ```
   import Distribution.Simple
   main = defaultMain
   ```

   Replace that with:

   ```
   import LiquidHaskell.Cabal
   main = liquidHaskellMain
   ```

   This hooks LiquidHaskell into your Cabal/Stack-based build.

   (For projects already using a custom `Setup.hs` file, see the section "Custom `Setup.hs` Files" below.)

1. Next, it's time to set up your project's .cabal file.

   Add `liquidhaskell-cabal` to the `build-depends` lists of each of your libraries and executables:

   ```
   library
     build-depends: {- ... other dependencies ... -}
                  , liquidhaskell-cabal >= 0.2.0
                    {- ... perhaps more dependencies? ... -}
   ```

   Then add a `custom-setup` stanza at the top level, outside the `library` and `executale` sections:

   ```
   custom-setup
     setup-depends: base, Cabal, liquidhaskell-cabal >= 0.2.0
   ```

   This tells Cabal to make the `base`, `Cabal`, and `liquidhaskell-cabal`
   packages available when building `Setup.hs`.  It goes at the top level of
   your .cabal file, next to `library`, `executable`, and `test-suite`. And as
   with other `setup-depends` lists, you can optionally set version bounds here.

   You'll also need a flag called `liquidhaskell` in your .cabal file;
   `liquidhaskell-cabal` only activates if it sees that a flag with this name is
   enabled. It is highly recommended that this be disabled by default, so that
   end users of your package don't need to know about LiquidHaskell to install
   it:

   ```
   flag liquidhaskell
     description: After building, verify with LiquidHaskell
     default:     False
   ```

   Finally, make sure the `build-type` field in your .cabal file is set to
   `Custom` (most projects use `Simple`):

   ```
   build-type: Custom
   ```

   Each library and executable in your package can optionally specify its own
   LiquidHaskell flags; see the section "Custom LiquidHaskell Flags" below for
   more on that.

1. When building, you may see a warning that looks like:

   ```
   Ignoring unknown section type: custom-setup
   ```

   This is because old versions of Cabal (before version 1.24) don't recognize the `custom-setup`
   stanza. If you're building with Stack, see the next step; otherwise, you'll need to install
   `liquidhaskell-cabal` manually to make it available to your `Setup.hs`:

   ```
   $ cabal install liquidhaskell-cabal-0.2.0.0
   ```

1. If you're building with Stack, add the following to your project's `stack.yaml`:

   ```
   extra-deps:
     - liquidhaskell-cabal-0.2.0.0
   ```

   (If your `stack.yaml` already has an `extra-deps` list, add
   `liquidhaskell-cabal-0.2.0.0` to the existing one instead of starting a
   second list.)

   Then, if you're using a version of Stack prior to 1.4, add the following as well:

   ```
   explicit-setup-deps:
     "*": true
   ```

   Otherwise, with Stack 1.4+, the `custom-setup` stanza in the `.cabal` file
   will be recognized automatically, and the `explicit-setup-deps` field is
   unnecessary.

That's it! You should be good to go.

## Usage

If you're using Stack, you can build and check with LiquidHaskell by adding
`--flag <package name>:liquidhaskell` to your `stack build` command:

```
stack build --flag mypackage:liquidhaskell
```

Otherwise, pass `-fliquidhaskell` to `cabal configure` to switch on
LiquidHaskell checking for your builds:

```
cabal configure -fliquidhaskell && cabal build
```

(Running `cabal configure` without `-fliquidhaskell` will turn it back off.)

## Custom `liquidhaskell-cabal` options

There are additional custom option fields that you can add to your `.cabal` file
for each executable or library:

### `x-liquidhaskell-options`
Extra command line flags to pass to LiquidHaskell (these are described in the [LiquidHaskell
README](https://github.com/ucsd-progsys/liquidhaskell#command-line-options)).
```
library
  (... other fields ...)
  x-liquidhaskell-options: --diff --no-termination

executable myexecutable
  (... other fields ...)
  x-liquidhaskell-options: --diff
```

### `x-liquidhaskell-checked-files`
Whitelist of files to check. This is convenient when you don't want to
check your entire project at once.

The list also supports directories, e.g. listing `A/B` will also include `A/B/C.hs`.
When this field is missing `liquidhaskell-cabal` defaults to checking everything.
```
library
  (... other fields ...)
  x-liquidhaskell-checked-files: src/A.hs src/B

executable myexecutable
  (... other fields ...)
  x-liquidhaskell-checked-files: app/Main.hs
```

## Custom `Setup.hs` Files

For most projects, the simple `Setup.hs` file given above (using
`liquidHaskellMain`) should be sufficient. However, for those already using
custom `Setup.hs` files, the `LiquidHaskell.Cabal` module
([Haddock](https://hackage.haskell.org/package/liquidhaskell-cabal-0.2.0.0/docs/LiquidHaskell-Cabal.html))
provides more granular means of hooking LiquidHaskell into the build process.

[`liquidHaskellHooks`](https://hackage.haskell.org/package/liquidhaskell-cabal-0.2.0.0/docs/LiquidHaskell-Cabal.html#v:liquidHaskellHooks)
is a Cabal `UserHooks` structure pre-configured for LiquidHaskell. Using it,
the basic `Setup.hs` file is equivalent to:

```haskell
import Distribution.Simple
import LiquidHaskell.Cabal
main = defaultMainWithHooks liquidHaskellHooks
```

[`liquidHaskellPostBuildHook`](https://hackage.haskell.org/package/liquidhaskell-cabal-0.2.0.0/docs/LiquidHaskell-Cabal.html#v:liquidHaskellPostBuildHook)
is the Cabal `postBuild` hook that actually configures and runs LiquidHaskell.
Using it, the above is equivalent to:

```haskell
import Distribution.Simple
import LiquidHaskell.Cabal
main = defaultMainWithHooks $
  simpleUserHooks { postBuild = liquidHaskellPostBuildHook }
```

Projects already using a `postBuild` hook can invoke
`liquidHaskellPostBuildHook` from within it, passing in the appropriate
arguments:

```haskell
import Distribution.Simple
import LiquidHaskell.Cabal

main = defaultMainWithHooks $
  simpleUserHooks { postBuild = myFancyHook }

myFancyHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myFancyHook args buildFlags pkgDesc lbi = do
  {- ... other important code ... -}
  liquidHaskellPostBuildHook args buildFlags pkgDesc lbi
  {- ... even more code ...-}
```

