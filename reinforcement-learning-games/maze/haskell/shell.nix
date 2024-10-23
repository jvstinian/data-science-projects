let
  pkgs = import <nixpkgs> {};
  # config = builtins.readFile /home/justinian/.config/nixpkgs/config.json;
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv: pkgs.haskell.lib.addBuildTools drv ((with pkgs.haskellPackages; [ cabal-install haskell-language-server ]) ++ (with pkgs; [ myNeovim ]));
  returnShellEnv = true;
  # overrides = self: super: {
  #   inline-c = pkgs.haskell.lib.dontCheck super.inline-c;
  # };
}
