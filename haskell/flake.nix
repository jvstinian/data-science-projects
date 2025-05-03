{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.ihaskell.url = "github:IHaskell/IHaskell";
  inputs.ihaskell.flake = false;
  inputs.flake-compat.url = "github:edolstra/flake-compat";

  nixConfig = {
    extra-substituters = [
      "https://ihaskell.cachix.org"
      "https://vaibhavsagar.cachix.org"
    ];
    extra-trusted-public-keys = [
      "ihaskell.cachix.org-1:WoIvex/Ft/++sjYW3ntqPUL3jDGXIKDpX60pC8d5VLM="
      "vaibhavsagar.cachix.org-1:PxFckJ8oAzgF4sdFJ855Fw38yCVbXmzJ98Cc6dGzcE0="
    ];
  };

  outputs = {self, nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      notebook = folder: {
        name = folder;
        path = import (./. + "/${folder}") { inherit system; };
      };
      notebooks = map notebook [
        "NGFS"
        "statistics"
      ];
    in {
      packages = builtins.listToAttrs (map (n: {name = n.name; value = n.path;}) notebooks);
      defaultPackage = pkgs.linkFarm "notebooks" notebooks;
    });
}
