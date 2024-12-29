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
      url = "github:jvstinian/zombpyg";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, libzombsole, libzombpyg, ... }: 
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

          # This is probably not the way I'll go
          train-script = pkgs.writeShellScriptBin "train.sh" ''
            python train_dqn.py --config zombsole_cnn
          '';

          my-process-bundle = pkgs.symlinkJoin {
              name = "my-process-bundle";
              buildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                  echo "NOTE: Links added in symlinkJoin"
                  makeWrapper ${train-script}/bin/train.sh $out/bin/my-wrapper --prefix PATH : $out/bin
              '';
              paths = [
                dev-python
              ];
          };
          
          # Will likely go this way instead
          python-train-app = pkgs.python3Packages.buildPythonApplication {
            pname = "dqn-train-example";
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
          pytrain = python-train-app;
        };
        apps.wrappertrain = {
          type = "app";
          program = "${my-process-bundle}/bin/my-wrapper";
        };
        apps.pytrain = {
          type = "app";
          program = "${python-train-app}/bin/train_dqn.py";
        };
      }
    );
}
