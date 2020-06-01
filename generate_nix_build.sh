#! /usr/bin/env nix-shell
#! nix-shell -p elm2nix -i bash

elm2nix convert > elm-srcs.nix
elm2nix snapshot > registry.dat

