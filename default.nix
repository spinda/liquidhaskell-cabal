{ mkDerivation, base, Cabal, filepath, stdenv }:
mkDerivation {
  pname = "liquidhaskell-cabal";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [ base Cabal filepath ];
  homepage = "https://github.com/spinda/liquidhaskell-cabal#readme";
  description = "Liquid Haskell integration for Cabal and Stack";
  license = stdenv.lib.licenses.bsd3;
}
