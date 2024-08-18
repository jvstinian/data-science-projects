let
  pkgs = import <nixpkgs> {};
  echopy = pkgs.callPackage ./build.nix {};
in pkgs.mkShell {
  packages = [
    (pkgs.python3.withPackages (python-pkgs: [ ]))
  ];
  buildInputs = [
    echopy
  ];
}
