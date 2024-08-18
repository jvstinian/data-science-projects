let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  # modifier = drv: pkgs.haskell.lib.addBuildTools drv (with pkgs; [ python38 ]) ;
  # overrides = self: super: {
  #   cpython = pkgs.haskell.lib.dontCheck super.cpython;
  # }
  returnShellEnv = false;
}
