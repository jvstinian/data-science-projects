# { pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {} }:
# { pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-24.05.tar.gz") {} }: # for v0.12.0
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/21808d22b1cda1898b71cf1a1beb524a97add2c4.tar.gz") {} }: # for v0.13.0

let 
  myNeovim = (pkgs.config.packageOverrides pkgs).myNeovim;
in 
pkgs.mkShell {
  packages = [ pkgs.zig pkgs.zls myNeovim ];
}
