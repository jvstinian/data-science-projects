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

          train-script = pkgs.writeShellScriptBin "train.sh" ''
            python train_dqn.py --config zombsole_cnn
          '';

          python-train-app = pkgs.python3Packages.buildPythonApplication {
            pname = "demo-train";
            version = "1.0";
            propagatedBuildInputs = dev-python-packages pkgs.python3Packages;
            src = ./.;
          };

          my-process-bundle = pkgs.symlinkJoin {
              name = "my-process-bundle";
              # meta.mainProgram = "process-example";
              buildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                  echo "NOTE: Links added in symlinkJoin"
                  makeWrapper ${train-script}/bin/train.sh $out/bin/my-wrapper --prefix PATH : $out/bin
              '';
              paths = [
                dev-python
              ];
          };

      in rec {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            dev-python
          ];
        };
        packages = {
          pytrain = python-train-app;
        #   zombpyg  = pkgs.python3Packages.zombpyg;
        #   # default  = pkgs.python3Packages.jvstinian-zombsole;
        };
        apps.train = {
          type = "app";
          # program = "${train-script}/bin/train.sh";
          program = "${my-process-bundle}/bin/my-wrapper";
        };
        apps.pytrain = {
          type = "app";
          program = "${python-train-app}/bin/train_dqn.py";
        };
        # apps.zombsole-stdio-json = {
        #   type = "app";
        #   program = "${packages.zombsole}/bin/zombsole-stdio-json";
        # };
      }
    );
}
