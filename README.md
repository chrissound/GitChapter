# Cabal Stack Awesome Init

## What problem does this solve?
Saves having to go through the tedious `cabal init` and adding a `.ghci file` to show all warnings.

## Instructions
Git clone this project, and run the following command (you'll need to modify the source and destination):

`rsync -av --progress ~/Projects/Haskell/CabalInit/ /path/To/Directory --exclude .git`

You can use the the `ghcid.sh` script to start a ghcid session. 
