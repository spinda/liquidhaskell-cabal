# liquidhaskell-cabal

Liquid Haskell integration for Cabal and stack.

The `LiquidHaskell.Cabal` module provides a kit to create `Setup.hs` files with
LiquidHaskell integration. A sample project configured with this package
[is available](https://github.com/spinda/liquidhaskell-cabal-demo).

The standard, basic `Setup.hs` configuration looks like this:

```haskell
import Distribution.Simple
main = defaultMain
```

A simple `Setup.hs` file that performs the standard behavior, with the addition
of LiquidHaskell support, can look like the following:

```haskell
import LiquidHaskell.Cabal
main = liquidHaskellMain
```

In order for Cabal/stack to properly execute your custom `Setup.hs` file, your
package's `.cabal` file will need to set the `build-type` to `Custom` (the
default is `Simple`):

```
build-type: Custom
```

A package with LiquidHaskell integration is expected to expose a flag called
`liquidhaskell`, which should normally default to disabled and can be
implemented by adding the following stanza to your project's `.cabal` file:

```
flag liquidhaskell
  description: After building, verify with LiquidHaskell
  default:     False
```

(The spacing and description are arbitrary and open to customization.)

During the post-build phase, the hook will check whether this flag is enabled
and, if it is, will run the LiquidHaskell binary with the appropriate command
line arguments and your package's source files. This assumes, of course, that
the LiquidHaskell binary is installed and available in `$PATH`.

Via stack, this flag can be enabled on the fly with
`--flag <project name>:liquidhaskell`; for example:

```
stack build --flag myproject:liquidhaskell
```

Running `cabal-install` directly, it will need to be enabled via `cabal
configure`:

```
cabal configure -fliquidhaskell && cabal build
```

When Cabal 1.24 releases, you will be able to add a stanza like the following
to your `.cabal` file to ensure `liquidhaskell-cabal` is installed and
available when the `Setup.hs` file is built:

```
custom-setup
  setup-depends: base, Cabal, liquidhaskell-cabal
```

Unfortunately Cabal &lt;1.24 has no means of tracking build dependencies for
`Setup.hs` files, so you will need to have `liquidhaskell-cabal` manually
installed before configuring/building your project. Using Cabal directly, this
can be done with `cabal install liquidhaskell-cabal`. Using stack, you can add
`liquidhaskell-cabal` to your `stack.yaml`'s `extra-deps` field.

Each component of the package (libary and executables) can specify its own
extra command line flags to pass to LiquidHaskell (these are described in the
[LiquidHaskell README](https://github.com/ucsd-progsys/liquidhaskell). Simply
add an `x-liquidhaskell-options` field to the relevant components:

```
library
  (... other fields ...)
  x-liquidhaskell-options: --diff --no-termination

executable myexecutable
  (... other fields ...)
  x-liquidhaskell-options: --diff
```

For most projects, the simple sample `Setup.hs` file given above, using
`liquidHaskellMain`, should be sufficient. However, for those already using
custom `Setup.hs` files, `liquidhaskell-cabal` exposes more granular means of
invoking the hook. Using `liquidHaskellHooks`, the basic `Setup.hs` file is
equivalent to:

```haskell
import Distribution.Simple
import LiquidHaskell.Cabal
main = defaultMainWithHooks liquidHaskellHooks
```

Using `liquidHaskellPostBuildHook`, this is further equivalent to:

```haskell
import Distribution.Simple
import LiquidHaskell.Cabal
main = defaultMainWithHooks $
  simpleUserHooks { postBuild = liquidHaskellPostBuildHook }
```

Projects already using a `postBuild` hook can invoke
`liquidHaskellPostBuildHook` from within this existing hook where
appropriate.

