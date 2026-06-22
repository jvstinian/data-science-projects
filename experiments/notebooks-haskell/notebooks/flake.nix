{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.ihaskell.url = "github:IHaskell/IHaskell";
  # inputs.ihaskell.inputs.nixpkgs24_11.follows = "nixpkgs";
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
      hmatrix-overlay = sel: sup: {
        # nix-filter = import pkgs.nix-filter; # Is this really the way to go?
        # nix-filter = nix-filter; # Is this really the way to go?
        haskell = sup.haskell // {
          packages = sup.haskell.packages // {
            ghc966 = sup.haskell.packages.ghc966.override {
              overrides = self: super: {
                # ghc-syntax-highlighter = self.ghc-syntax-highlighter_0_0_12_0;
                # hmatrix-gsl = sup.haskell.lib.addBuildDepends (sup.haskell.lib.enableCabalFlag (sup.haskell.lib.dontStrip super.hmatrix-gsl) "disable-default-paths") [ sup.gsl ];
                hmatrix-gsl = sup.haskell.lib.addBuildDepends (sup.haskell.lib.dontStrip super.hmatrix-gsl) [ sup.gsl ];
              };
            };
          };
        };
      };
      hsoverlay = import "${ihaskell}/nix/overlay-9.6.nix";
      pkgs = import nixpkgs {
        inherit system;
        # overlays = [ overlay nix-filter.overlays.default ]; # overlay 
        overlays = [ hmatrix-overlay hsoverlay nix-filter.overlays.default ]; # overlay 
      };
      jupyterlab = pkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
      jupyterlab-package = pkgs.callPackage "${ihaskell}/nix/release.nix" { compiler = "ghc966"; }{
        extraEnvironmentBinaries = [jupyterlab]; # adding pkgs.gsl here didn't help # pkgs.pkg-config
        # extraEnvironmentBinaries = [jupyterlab pkgs.gsl pkgs.pkg-config];
        packages = self: with self; [Chart Chart-diagrams ihaskell-charts word8 statistics hmatrix hmatrix-gsl normaldistribution cassava];
        # systemPackages = sps: with sps; [ gsl ]; # This doesn't seem to help
        # staticExecutable = true;
      };
      # jupyterlab-package-modified = pkgs.haskell.lib.addBuildTools jupyterlab-package (with pkgs.haskell.packages.ghc966; [cabal-install]);
 
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
        jupyterlab = jupyterlab-package;
        default = jupyterlab-package;
        hmatrix-gsl = pkgs.haskell.packages.ghc966.hmatrix-gsl;
      };
      apps = {
        jupyterlab-notebook = {
            type = "app";
            program = "${self.packages.${system}.jupyterlab}/bin/jupyter-notebook";
	};
        default = self.apps.${system}.jupyterlab-notebook;
      };
      devShell = pkgs.mkShell {
        packages = [ jupyterlab-package ];
        # inputsFrom = [ jupyterlab-package ];
        shellHook = "export PS1='\\[\\e[1;34m\\]ihaskell-dev > \\[\\e[0m\\]'";
      };
    });
}
