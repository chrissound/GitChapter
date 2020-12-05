#!/usr/bin/env bash
if which nix
then
  if [ $# -eq 0 ]
  then
    nix-shell --run "ghcid --command='cabal v2-repl gitchapter' --test=Main.main"
    exit 0
  elif [ "$1" = "--check-only" ]
  then
    nix-shell --run "ghcid --command='cabal v2-repl gitchapter'"
    exit 0
  fi
fi
if which stack
then
    ghcid '--command=stack ghci gitchapter:exe:gitchapter' --test='Main.main'
    exit 0
fi

