{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
