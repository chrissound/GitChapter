#!/bin/bash
ghcid '--command=stack ghci -j 1 app:exe:test' --test='Test.main';

