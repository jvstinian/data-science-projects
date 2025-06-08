{
  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://cuda-maintainers.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
    ];
  };

  inputs = {
    nixpkgs = {
      # url = "github:nixos/nixpkgs/0c0bf9c057382d5f6f63d54fd61f1abd5e1c2f63";
      url = "github:nixos/nixpkgs/nixos-25.05";
    };
    libzombsole = {
      url = "github:jvstinian/libzombsole/nixos-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    libzombpyg = {
      url = "github:jvstinian/zombpyg/nixos-25.05";
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
          cuda_enable_overlay = final: prev: {
              config = prev.config // { cudaSupport = true; };
          };
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ libzombsole.overlays.default libzombpyg.overlays.default libprlpdemo.overlays.default cuda_enable_overlay ];
          };
    
          dev-python-packages = ps: with ps; [
              numpy
              # opencv4
              scikit-learn
              tensorflowWithCuda keras # 2025-06-06: Adding keras to avoid recursion issue
              gymnasium # pulls in tensorflow-gpu which is not compatible with python-3.12
              jvstinian-zombsole
              zombpyg
	      prlp-demo
              torch
              torchvision
              jupyter
              ipython
              pandas # TODO: Is this package and the following needed?
              statsmodels
              matplotlib
          ];
          dev-python = pkgs.python3.withPackages dev-python-packages;

          # Will likely go this way instead
          # python-train-app = pkgs.python3Packages.buildPythonApplication {
          #   pname = "dqn-train-examples";
          #   version = "1.0";
          #   propagatedBuildInputs = dev-python-packages pkgs.python3Packages;
          #   src = ./.;
          # };
      in rec {
        devShell = pkgs.mkShell {
          # buildInputs = with pkgs; [
          #   dev-python # cudaPackages.cudatoolkit cudaPackages.cudnn
          # ];
          packages = [ dev-python ];
          # inputsFrom = with pkgs; [
          #   cudaPackages.cudatoolkit cudaPackages.cudnn
          # ];
          shellHook = ''
            export PS1='\[\e[1;34m\]pytorch-dqn-example > \[\e[0m\]'
            export XLA_FLAGS="--xla_gpu_cuda_data_dir=${pkgs.cudatoolkit}/"
            # jupyter-notebook --ip=10.0.0.100 --port 8888 --no-browser
          '';
        };
        # packages = {
        #   dqn-train = python-train-app;
        # };
        # apps = {
	#   dqn-train = {
	#     type = "app";
        #     program = "${python-train-app}/bin/train_dqn.py";
        #   };
	#   dqn-eval = {
	#     type = "app";
        #     program = "${python-train-app}/bin/eval_dqn.py";
        #   };
	# };
      }
    );
}
