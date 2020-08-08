if which nix; then
    nix-shell --run "ghcid --command='cabal v2-repl gitchapter' --test=Main.main"
    exit 0
fi
if which stack; then
    ghcid '--command=stack ghci gitchapter:exe:gitchapter' --test='Main.main'
    exit 0
fi

