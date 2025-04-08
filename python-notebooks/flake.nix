# NOTE: Had a name collision between the flake input zombpyg and the zombpyg python package added by overlay, so renaming the input to libzombpyg
{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-23.11";
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
            overlays = [ ];
          };
    
          dev-python-packages = ps: with ps; [
              numpy
              opencv4
              tensorflowWithCuda
              keras
              jupyter
              ipython
              pandas
              numpy
              statsmodels
              seaborn
              tabulate # for writing pandas dataframes to markdown
              xgboost
              scikit-learn
          ];
          dev-python = pkgs.python3.withPackages dev-python-packages;

      in rec {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            dev-python
          ];
          shellHook = "export PS1='\\[\\e[1;34m\\]jupyter > \\[\\e[0m\\]'";
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
