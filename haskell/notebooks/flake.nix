{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.ihaskell.url = "github:IHaskell/IHaskell";
  inputs.ihaskell.flake = false;
  inputs.flake-compat.url = "github:edolstra/flake-compat";

  nixConfig = {
    extra-substituters = [
      "https://ihaskell.cachix.org"
      "https://vaibhavsagar.cachix.org"
    ];
    extra-trusted-public-keys = [
      "ihaskell.cachix.org-1:WoIvex/Ft/++sjYW3ntqPUL3jDGXIKDpX60pC8d5VLM="
      "vaibhavsagar.cachix.org-1:PxFckJ8oAzgF4sdFJ855Fw38yCVbXmzJ98Cc6dGzcE0="
    ];
  };

  outputs = {self, nixpkgs, nix-filter, flake-utils, ihaskell, ...}:
    flake-utils.lib.eachDefaultSystem (system: let
      overlay = sel: sup: {
        # nix-filter = import pkgs.nix-filter; # Is this really the way to go?
        # nix-filter = nix-filter; # Is this really the way to go?
        haskell = sup.haskell // {
          packages = sup.haskell.packages // {
            ghc948 = sup.haskell.packages.ghc948.override {
              overrides = self: super: {
                ghc-syntax-highlighter = self.ghc-syntax-highlighter_0_0_10_0;
              };
            };
          };
        };
      };
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ overlay nix-filter.overlays.default ];
      };
      jupyterlab = pkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
      jupyterlab_package = pkgs.callPackage "${ihaskell}/nix/release.nix" { compiler = "ghc948"; }{
        extraEnvironmentBinaries = [jupyterlab];
        packages = self: with self; [Chart Chart-diagrams ihaskell-charts];
      };
 
      # pkgs = nixpkgs.legacyPackages.${system};
      # notebook = folder: {
      #   name = folder;
      #   path = import (./. + "/${folder}") { inherit system; };
      # };
      # notebooks = map notebook [
      #   # "callcc" "chart-diagrams" "codensity" "continuations" "deriving-via" "docker" "dragon-curve" "efficient-combinator-parsers" "git-from-scratch" "graphviz" "hamt" "higher-kinded-data" "hs-updater" "intmap" "lambda" "mph" "refactoring-tarjan" "revisiting-monadic-parsing-haskell" "revisiting-poor-mans-concurrency" "smt" "solver" "tarjan" "trees-that-shrink" "typeclasses" "zulip-api"
      #   "NGFS"
      # ];
    in {
      # packages = builtins.listToAttrs (map (n: {name = n.name; value = n.path;}) notebooks);
      # defaultPackage = pkgs.linkFarm "notebooks" notebooks;
      packages = {
        nix-filter = pkgs.nix-filter;
        jupyterlab = jupyterlab_package;
      };
      apps = {
        jupyterlab-notebook = {
            type = "app";
            program = "${self.packages.${system}.jupyterlab}/bin/jupyter-notebook";
	};
        default = self.apps.${system}.jupyterlab-notebook;
      };
    });
}
