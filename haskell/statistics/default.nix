{ system ? builtins.currentSystem }:
let
  pkgs = import ../pkgs.nix;
  overlay = sel: sup: {
    nix-filter = import pkgs.nix-filter;
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc948 = sup.haskell.packages.ghc948.override {
          overrides = self: super: {
            ghc-syntax-highlighter = self.ghc-syntax-highlighter_0_0_10_0;
            local-statistical-models = sup.haskell.packages.ghc948.callPackage ./statistical-models/build.nix { };
          };
        };
      };
    };
  };
  nixpkgs = (import pkgs.nixpkgs { inherit system; overlays = [ overlay ]; });
  # local-statistical-models = pkgs.callPackage ../../statistical-models/build.nix { inherit (nixpkgs.haskell.packages.ghc948) ad vector math-functions; }
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
in nixpkgs.callPackage "${pkgs.ihaskell}/nix/release.nix" { compiler = "ghc948"; }{
  extraEnvironmentBinaries = [jupyterlab];
  # packages = self: (with self; [local-statistical-models statistics]);
  packages = self: with self; [word8 statistics local-statistical-models Chart Chart-diagrams ihaskell-charts];
}
