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
      url = "github:nixos/nixpkgs/nixos-25.05";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, ... }: 
      flake-utils.lib.eachDefaultSystem (system:
        let 
          cuda_enable_overlay = final: prev: {
              config = prev.config // { cudaSupport = true; };
          };
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ cuda_enable_overlay ];
          };
    
          dev-python-packages = ps: with ps; [
              numpy pandas statsmodels matplotlib
              # opencv4
              scikit-learn scikit-image
              tensorflowWithCuda keras
              torch torchvision
              gymnasium pygame
              jupyter ipython
          ];
          dev-python = pkgs.python3.withPackages dev-python-packages;
      in rec {
        devShell = pkgs.mkShell {
          packages = [ dev-python ];
          shellHook = ''
            export PS1='\[\e[1;34m\]tutorial-notebooks > \[\e[0m\]'
            export XLA_FLAGS="--xla_gpu_cuda_data_dir=${pkgs.cudatoolkit}/"
            jupyter-notebook --ip=10.0.0.100 --port 8888 --no-browser
          '';
        };
      }
    );
}
