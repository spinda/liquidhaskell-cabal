let
nixpkgs = import ./nix/nixpkgs.nix;

config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
            overrides = haskellPackagesNew : haskellPackagesOld : {
                liquidhaskell-cabal =
                    haskellPackagesNew.callPackage ./default.nix { };
            };
        };
    };
};


pkgs = import nixpkgs { inherit config; };

in
  { liquidhaskell-cabal = pkgs.haskellPackages.liquidhaskell-cabal;
  }
