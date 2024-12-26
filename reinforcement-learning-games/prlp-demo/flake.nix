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
    let
      python-prlp-demo-overlay = final: prev: {
          pythonPackagesOverlays = (prev.pythonPackagesOverlays or [ ]) ++ [
              (python-final: python-prev: {
                  prlp-demo = python-final.buildPythonPackage rec {
                      name = "prlp-demo";
                      src = ./.;
    
                      # was previously using "dependencies" but the packages 
                      # didn't appear to propagate to the output package
                      propagatedBuildInputs = with python-final; [
                        numpy
                        pygame
                      ];
    
                      doCheck = true;
                      # Including pytestCheckHook in nativeCheckInputs to run pytest. 
                      # If needed, arguments can be passed to pytest using pytestFlagsArray.  
                      # Alternatively, checkPhase can be explicitly provided.
                      # See https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/python.section.md#using-pytestcheckhook 
                      # for more details.
                      nativeCheckInputs = with python-final; [
                        # pytestCheckHook 
                      ];
                  };
              })
          ];

          python3 = let
                  self = prev.python3.override {
                      inherit self;
                      packageOverrides = prev.lib.composeManyExtensions final.pythonPackagesOverlays;
                  }; 
              in self;

          python3Packages = final.python3.pkgs;
      };
    in 
      flake-utils.lib.eachDefaultSystem (system:
        let 
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ python-prlp-demo-overlay ];
          };
    
          dev-python-packages = ps: with ps; [
              pygame
              gym
              prlp-demo
          ];
          dev-python = pkgs.python3.withPackages dev-python-packages;
      in rec {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            dev-python
          ];
        };
        packages = {
          prlp-demo = pkgs.python3Packages.prlp-demo;
          default = pkgs.python3Packages.prlp-demo;
        };
      }
    ) // {
      overlays.default = python-prlp-demo-overlay;
    };
}
