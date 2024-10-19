let pkgs = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/205fd4226592cc83fd4c0885a3e4c9c400efabb5.tar.gz") { overlays = [ (builtins.getFlake github:jvstinian/libzombsole/flake-add-overlay).overlays.default ]; };
  #         [
  #             (final: prev: {
  #       	  pythonPackagesOverlays = (prev.pythonPackagesOverlays or [ ]) ++ [
  #       	      (python-final: python-prev: {
  #       		  zombsole = 
  #       	      })
  #       	  ];

  #       	  python310 = let
  #       		  self = prev.python310.override {
  #       		      inherit self;
  #       		      packageOverrides = prev.lib.composeManyExtensions final.pythonPackagesOverlays;
  #       		  }; 
  #       	      in self;

  #       	  python310Packages = final.python310.pkgs;
  #             })
  #         ]; };
  # https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/python.section.md
  # jvstinian-zombsole = (builtins.getFlake github:jvstinian/libzombsole).packages.x86_64-linux.default;
  # jvstinian-zombsole = pkgs.python310Packages.buildPythonPackage rec {
  #     name = "zombsole";
  #     # version = "0796721c36e3133e00d7f4c7fb553be33b8556c0";

  #     src = pkgs.fetchFromGitHub {
  #              owner = "jvstinian";
  #        repo = "${name}";
  #        rev = "${version}";
  #        sha256 = "RehO2tDY8nqlSxByCYpfditWyfJ7cnmZUhUGaE6oWeM=";
  #     };
  #     
  #     dependencies = with pkgs.python310.pkgs; [
  #       docopt termcolor 
  #     ];

  #     # nativeBuildInputs = with pkgs.python310.pkgs; [
  #     #   docopt termcolor 
  #     # ];

  #     nativeCheckInputs = with pkgs.python310.pkgs; [
  #       requests gym flask docopt termcolor # is pip required?
  #     ];

  #     # buildInputs = with pkgs.python310.pkgs; [ docopt ];

  #     # build-system = with pkgs.python310.pkgs; [ setuptools pip ];
  #     # propagatedBuildInputs = with pkgs.python3Packages; [ setproctitle ];
  #     doCheck = true; # tests failing

  #     meta = {
  #       homepage = "https://github.com/jvstinian/zombsole";
  #       description = "Description here.";
  #       license = pkgs.lib.licenses.mit;
  #       maintainers = [ "jvstinian" ];
  #     };
  #   };
  my-python-packages = ps: with ps; [
    # docopt
    # termcolor
    numpy
    # tensorflow
    # opencv4
    # gym
    jvstinian-zombsole
    python-lsp-server
  ];
  # my-python = jvstinian-zombsole.dependencies ++ (pkgs.python310.withPackages my-python-packages);
  my-python = pkgs.python310.withPackages my-python-packages;
in 
# my-python.env
pkgs.mkShell {
  # Putting jvstinian-zombsole in the following works
  # packages = (with pkgs; [ haskell-language-server ghc myNeovim ]) ++ [ my-python ];
  packages = with pkgs; [ haskell-language-server ghc myNeovim ];
  buildInputs = [
      my-python
  ];
  shellHook=''
  python
  '';
}

