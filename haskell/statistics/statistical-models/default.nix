let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
in
# pkgs.haskell.packages.ghc92.developPackage
pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv: pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [ cabal-install ]) ;
  # overrides = self: super: {
  #   continued-fractions = pkgs.haskell.lib.dontCheck super.continued-fractions;
  # };
  returnShellEnv = true;
}
