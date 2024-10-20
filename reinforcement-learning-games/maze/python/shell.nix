# References:
# * https://wiki.nixos.org/wiki/Overlays for using overlays with nix-shell
# Also, initially I was using "import <nixpkgs> ..." but I found that the python installation could not import zombsole.
# Eventually I realized this might be because the zombsole flake is pinned to a different version of nixpkgs.  
# I went into the flake lock file for libzombsole and pulled out the revision, and am pinning to that version of 
# nixpkgs in this nix expression.  
# TODO: Add the moment we're using the overlay that has been added to the zombsole flake in a branch.  We will need 
#       to point this to the master branch at some point.
let pkgs = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/205fd4226592cc83fd4c0885a3e4c9c400efabb5.tar.gz") { overlays = [ (builtins.getFlake github:jvstinian/libzombsole/read-maps-by-name).overlays.default ]; };
  # originally had this following for overlays
  #         [
  #             (final: prev: {
  #       	  pythonPackagesOverlays = (prev.pythonPackagesOverlays or [ ]) ++ [
  #       	      (python-final: python-prev: {
  #       		  zombsole =(builtins.getFlake github:jvstinian/libzombsole/flake-add-overlay).packages.x86_64-linux.default;
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
  dev-python-packages = ps: with ps; [
    numpy
    jvstinian-zombsole
    python-lsp-server
  ];
  dev-python = pkgs.python310.withPackages dev-python-packages;
in 
pkgs.mkShell {
  packages = with pkgs; [ haskell-language-server ghc ];
  buildInputs = [
      dev-python
  ];
  # shellHook=''
  # python
  # '';
}

