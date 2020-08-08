#!/bin/bash
if which nix; then
    nix-shell --run "ghcid --command='cabal v2-repl gitchapter' --test=Test.main"
    exit 0
fi
if which stack; then
    ghcid '--command=stack ghci -j 1 gitchapter:exe:gitchapter' --test='Test.main'
    exit 0
fi

