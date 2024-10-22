{
  inputs = {
    nixpkgs = {
      # url = "github:nixos/nixpkgs/nixos-23.11";
      follows = "libzombsole/nixpkgs";
    };
    libzombsole = {
      url = "github:jvstinian/libzombsole/flake-python3";
    };
    zombpyg = {
      url = "github:jvstinian/zombpyg";
      inputs.nixpkgs.follows = "libzombsole/nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, libzombsole, zombpyg, ... }: 
    let
        placeholder-value = "hell";
    in 
      flake-utils.lib.eachDefaultSystem (system:
        let 
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ libzombsole.overlays.default ];
          };
    
          dev-python-packages = ps: with ps; [
              numpy
              opencv4
              tensorflowWithCuda
              gym
              jvstinian-zombsole
	      zombpyg.packages.${system}.zombpyg
          ];
          dev-python = pkgs.python3.withPackages dev-python-packages;
      in rec {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            dev-python
          ];
        };
        # packages = {
        #   zombsole = pkgs.python3Packages.jvstinian-zombsole;
        #   default = pkgs.python3Packages.jvstinian-zombsole;
        # };
        # apps.default = {
        #   type = "app";
        #   program = "${packages.zombsole}/bin/zombsole";
        # };
        # apps.zombsole-stdio-json = {
        #   type = "app";
        #   program = "${packages.zombsole}/bin/zombsole-stdio-json";
        # };
      }
    );
}
