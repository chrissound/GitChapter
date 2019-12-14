{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" } :
let
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
    };
  };
in
  myHaskellPackages.callCabal2nix "gitchaper" (./.) {}
