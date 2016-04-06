# liquidhaskell-cabal

*Liquid Haskell integration for Cabal and stack.*

[![Hackage](https://img.shields.io/hackage/v/liquidhaskell-cabal.svg)](https://hackage.haskell.org/package/liquidhaskell-cabal)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/liquidhaskell-cabal.svg)](http://packdeps.haskellers.com/feed?needle=liquidhaskell-cabal)

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
implemented by adding the following stanza to your package's `.cabal` file:

```
flag liquidhaskell
  description: After building, verify with LiquidHaskell
  default:     False
```

(The spacing and description are arbitrary and open to customization.)

During the post-build phase, the hook will check whether this flag is enabled
and, if it is, will run the LiquidHaskell binary with the appropriate command
line arguments and your package's source files. This assumes, of course, that
the LiquidHaskell binary is installed and available in `$PATH` (please make
sure you have a version >= 0.6.0.0 installed).

Via stack, this flag can be enabled on the fly with
`--flag <package name>:liquidhaskell`; for example:

```
stack build --flag mypackage:liquidhaskell
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
`Setup.hs` files. If you're using Cabal directly, you'll need to install
`liquidhaskell-cabal` manually before configuring/building your project:

```
cabal install liquidhaskell-cabal
```

If you're using stack, add `liqudhaskell-cabal` to each of your components'
`build-depends`, add `liquidhaskell-cabal-0.1.0.0` to your `stack.yaml`'s
[`extra-deps` section](https://github.com/commercialhaskell/stack/blob/master/doc/yaml_configuration.md#extra-deps),
and add an
[`explicit-setup-deps`](https://github.com/commercialhaskell/stack/blob/master/doc/yaml_configuration.md#explicit-setup-deps)
section to your `stack.yaml`:

```
explicit-setup-deps:
  "*": true
extra-deps:
- liquidhaskell-cabal-0.1.0.0
```

Then you can build your project as you normally would with stack.

Each component of the package (library and executables) can specify its own
extra command line flags to pass to LiquidHaskell (these are described in the
[LiquidHaskell README](https://github.com/ucsd-progsys/liquidhaskell)). Simply
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

