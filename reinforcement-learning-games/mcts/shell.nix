{
  pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/d3c42f187194c26d9f0309a8ecc469d6c878ce33.tar.gz") {} # unstable
}: 
let
  my-ghc = (pkgs.haskellPackages.ghcWithPackages (hs: [hs.cabal-install hs.mtl hs.random hs.mwc-random]));
    # hs.GeBoP
in 
pkgs.mkShell {
  packages = [ my-ghc ] ++ (with pkgs; [ myNeovim ]);
  # nativeBuildInputs = with pkgs; [ pkg-config ];
  # inputsFrom = [ pkgs.SDL2 my-python ];
}

