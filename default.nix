{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" } :
let
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
    pandoc-include-code  = self.callCabal2nix "pandoc-include-code" (builtins.fetchGit {
        url = "git@github.com:owickstrom/pandoc-include-code.git";
        rev = "3afe94299b3a473fda0c62fdfd318435117751dd";
      })
      {};
    };
  };
in
  myHaskellPackages.callCabal2nix "gitchaper" (./.) {}
