# NOTE: Had a name collision between the flake input zombpyg and the zombpyg python package added by overlay, so renaming the input to libzombpyg
{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-23.11";
    };
    libzombsole = {
      url = "github:jvstinian/libzombsole";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    libzombpyg = {
      url = "github:jvstinian/zombpyg/flake-refactor-overlay"; # TODO: Change after merge
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, libzombsole, libzombpyg, ... }: 
    let
        placeholder-value = "hell";
    in 
      flake-utils.lib.eachDefaultSystem (system:
        let 
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ libzombsole.overlays.default libzombpyg.overlays.default ];
          };
    
          dev-python-packages = ps: with ps; [
              numpy
              opencv4
              tensorflowWithCuda
              gym
              jvstinian-zombsole
              zombpyg 
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
        #   zombpyg  = pkgs.python3Packages.zombpyg;
        #   # default  = pkgs.python3Packages.jvstinian-zombsole;
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
