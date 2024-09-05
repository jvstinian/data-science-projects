# process-example

## NixOS

### shell

To run this example, from this directory, use nix shell: 
```
nix-shell shellfor.nix
```

One can then run the executable with `cabal run`.

### flake

To enter a nix shell with the necessary dependencies, use 
```
nix develop
```

To build the `process-example`, use 
```
nix build
```
In order to successfully run the resulting executable, 
the executable `zombsole-stdio-json` must be available available on the path.

