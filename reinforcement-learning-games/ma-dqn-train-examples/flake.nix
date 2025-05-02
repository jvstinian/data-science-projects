# NOTE: Had a name collision between the flake input zombpyg and the zombpyg python package added by overlay, so renaming the input to libzombpyg
{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-23.11";
    };
    libzombpyg = {
      url = "github:jvstinian/zombpyg/gym-env-renaming";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, libzombpyg, ... }: 
      flake-utils.lib.eachDefaultSystem (system:
        let 
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ libzombpyg.overlays.default ];
          };
    
          dev-python-packages = ps: with ps; [
              numpy
              opencv4
              tensorflowWithCuda
	      keras
              gym
              zombpyg
          ];
          dev-python = pkgs.python3.withPackages dev-python-packages;

          python-train-app = pkgs.python3Packages.buildPythonApplication {
            pname = "ma-dqn-train-examples";
            version = "1.0";
            propagatedBuildInputs = dev-python-packages pkgs.python3Packages;
            src = ./.;
          };
      in rec {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            dev-python
          ];
          shellHook = "export PS1='\\[\\e[1;34m\\]ma-dqn-train-dev > \\[\\e[0m\\]'";
        };
        packages = {
          ma-dqn-train = python-train-app;
        };
        apps = {
	  ma-dqn-train = {
	    type = "app";
            program = "${python-train-app}/bin/train_dqn.py";
          };
	  ma-dqn-eval = {
	    type = "app";
            program = "${python-train-app}/bin/eval_dqn.py";
          };
        };
      }
    );
}
