with import <nixpkgs> {};
# We add our packages to the haskell package set
let
  echopy = pkgs.callPackage ./echopy/build.nix {};
in
  (haskellPackages.extend (haskell.lib.compose.packageSourceOverrides {
    process-example = ./.;
  })).shellFor {
      # We call on this set shellFor to drop us into a shell containing the dependencies of frontend and backend:
      packages = p: [p.process-example];
      withHoogle = true;
      buildInputs = [ pkgs.cabal-install echopy ];
  }
