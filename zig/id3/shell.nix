# { pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {} }:
# { pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-24.05.tar.gz") {} }: # for v0.12.0
# { pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/21808d22b1cda1898b71cf1a1beb524a97add2c4.tar.gz") {} }: # for v0.13.0
# { pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/12a55407652e04dcf2309436eb06fef0d3713ef3.tar.gz") {} }: # for v0.14.0
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/e6f23dc08d3624daab7094b701aa3954923c6bbb.tar.gz") {} }: # for v0.14.1

let 
  myNeovim = (pkgs.config.packageOverrides pkgs).myNeovim;
in 
pkgs.mkShell {
  packages = [ pkgs.zig pkgs.zls myNeovim ];
}
