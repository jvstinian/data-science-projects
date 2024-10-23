# Maze Path Finder Using Policy Iteration

We implement the policy iteration algorithm from section 3.2.2 of
"Reinforcement Learning: A Survey" by Kaelbling, Littman, and Moore (1996)
in Haskell.

## Run Using Nix

A development environment is set up in shell.nix.  The application can be run using

```
nix-shell shell.nix
``` 

which will drop you into an environment with the necessary development and build tools.
In this environment, `cabal` is used to build and run the application.  This can be done
for instance with
```
cabal run
```

