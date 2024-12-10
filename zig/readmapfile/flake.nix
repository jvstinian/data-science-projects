{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, ... }: 
      flake-utils.lib.eachDefaultSystem (system:
        let 
          pkgs = import nixpkgs {
            inherit system;
          };
          read-map-file-zig = pkgs.stdenv.mkDerivation {
            pname = "read-map-file-zig";
            version = "0.1.0";
            src = ./.;
            description = "Zig example for reading zombsole maps";

            nativeBuildInputs = [
              pkgs.zig.hook
            ];
          };
        in rec {
          devShell = pkgs.mkShell {
            buildInputs = with pkgs; [
              zig zls
            ];
          };
          packages = {
            default = read-map-file-zig;
          };
          apps.default = {
            type = "app";
            program = "${packages.default}/bin/readmapfile";
          };
        }
      );
}
