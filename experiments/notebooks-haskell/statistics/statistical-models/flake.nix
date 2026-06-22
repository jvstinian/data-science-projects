{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";
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
      # hmatrix-overlay = sel: sup: {
      #   haskell = sup.haskell // {
      #     packages = sup.haskell.packages // {
      #       ghc966 = sup.haskell.packages.ghc966.override {
      #         overrides = self: super: {
      #           # ghc-syntax-highlighter = self.ghc-syntax-highlighter_0_0_12_0;
      #           # hmatrix-gsl = sup.haskell.lib.addBuildDepends (sup.haskell.lib.enableCabalFlag (sup.haskell.lib.dontStrip super.hmatrix-gsl) "disable-default-paths") [ sup.gsl ];
      #           hmatrix-gsl = sup.haskell.lib.addBuildDepends (sup.haskell.lib.dontStrip super.hmatrix-gsl) [ sup.gsl ];
      #         };
      #       };
      #     };
      #   };
      # };
      hsoverlay = import "${ihaskell}/nix/overlay-9.12.nix";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ hsoverlay nix-filter.overlays.default ]; # overlay 
      };
      jupyterlab = pkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
      jupyterlab-package = pkgs.callPackage "${ihaskell}/nix/release.nix" { compiler = "ghc912"; }{
        extraEnvironmentBinaries = [jupyterlab]; # adding pkgs.gsl here didn't help # pkgs.pkg-config
        packages = self: with self; [Chart Chart-diagrams ihaskell-charts word8 statistics hmatrix hmatrix-gsl normaldistribution cassava]; -- dataframe
        # extraEnvironmentBinaries = [jupyterlab pkgs.gsl pkgs.pkg-config];
        # systemPackages = sps: with sps; [ gsl ]; # This doesn't seem to help
        # staticExecutable = true;
      };
    in {
      packages = {
        nix-filter = pkgs.nix-filter;
        jupyterlab = jupyterlab-package;
        default = jupyterlab-package;
        # hmatrix-gsl = pkgs.haskell.packages.ghc966.hmatrix-gsl;
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
