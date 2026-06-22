# Haskell Packages and Notebooks

## Haskell Notebooks

Initially we followed [notebooks with ihaskell on nix](https://github.com/vaibhavsagar/notebooks),
but the approach creaates separate notebook instances in each subdirectory.
For our purposes, at the moment, a single jupyter notebook installation with numerous packages
should suffice.
This approach of maintaining a single jupyter notebook server instance is reflected in `./notebooks`.

The server-per-folder is implemented using `./flake.nix` and the subdirectories `NGFS` and `statistics`.
These will be removed once we've verified the single server approach.

## Haskell Packages

We are starting to work on a package `statistical-models`.  This is currently under `statistics`, but
the plan is to move this to a new subdirectory `packages`.

