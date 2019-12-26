{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc865"
, sources ? import ./nix/sources.nix
} :

let
  niv = import sources.nixpkgs {
    overlays = [
      (_ : _ : { niv = import sources.niv {}; })
    ] ;
    config = {};
  };
  pkgs = niv.pkgs;
  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # broken again
      # https://github.com/NixOS/nixpkgs/issues/42073
      pandoc-include-code  = self.callCabal2nix "pandoc-include-code" (builtins.fetchGit {
        url = "git@github.com:owickstrom/pandoc-include-code.git";
        rev = "7e4d9d967ff3e3855a7eae48408c43b3400ae6f4";
      })
        {};

    };
  };
in
  myHaskellPackages.callCabal2nix "gitchaper" (./.) {}
