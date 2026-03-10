# NOTE: Had a name collision between the flake input zombpyg and the zombpyg python package added by overlay, so renaming the input to libzombpyg
{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-24.05";
    };
    libzombsole = {
      url = "github:jvstinian/libzombsole";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    libzombpyg = {
      url = "github:jvstinian/zombpyg";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    libprlpdemo = {
      url = "github:jvstinian/data-science-projects?dir=reinforcement-learning-games/prlp-demo";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, libzombsole, libzombpyg, libprlpdemo, ... }: 
      flake-utils.lib.eachDefaultSystem (system:
        let 
          # TODO: As noted below, the keras override seems to create a problem.
          #       On the other hand, including keras without modification seems to 
          #       resolve the missing module we were seeing with tensorflow.
          python-keras-overlay = final: prev: {
              python3 = prev.python3.override {
                  # The keras override seems to cause a problem
                  packageOverrides = python-final: python-prev: {
		      keras = python-prev.keras.override { tensorflow = python-prev.tensorflowWithCuda; };
                  };
              }; 
          };
    
          pkgs = import nixpkgs {
            inherit system;
            # overlays = [ libzombsole.overlays.default libzombpyg.overlays.default libprlpdemo.overlays.default python-keras-overlay ];
            overlays = [ libzombsole.overlays.default libzombpyg.overlays.default libprlpdemo.overlays.default ];
          };

          dev-python-packages = ps: with ps; [
              numpy
              opencv4
              tensorflowWithCuda
              gymnasium # keras  # TODO: Is keras still used?
              jvstinian-zombsole
              prlp-demo
              zombpyg 
              jupyter
              ipython
          ];
          dev-python = pkgs.python3.withPackages dev-python-packages;

          # Will likely go this way instead
          python-train-app = pkgs.python3Packages.buildPythonApplication {
            pname = "dqn-train-examples";
            version = "1.0";
            propagatedBuildInputs = dev-python-packages pkgs.python3Packages;
            src = ./.;
          };
      in rec {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            dev-python
          ];
          shellHook = "export PS1='\\[\\e[1;34m\\]dqn-train-dev > \\[\\e[0m\\]'";
        };
        packages = {
          dqn-train = python-train-app;
        };
        apps = {
	  dqn-train = {
	    type = "app";
            program = "${python-train-app}/bin/train_dqn.py";
          };
	  dqn-eval = {
	    type = "app";
            program = "${python-train-app}/bin/eval_dqn.py";
          };
	};
      }
    );
}
