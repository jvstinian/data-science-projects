{
  # copied from https://github.com/NixOS/templates/blob/master/haskell-hello/flake.nix
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "A simple Haskell application using subprocess to interact with zombsole.  Includes package, overlay, and devShell.";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
  inputs.libzombsole.url = "github:jvstinian/libzombsole";
  outputs = { self, nixpkgs, libzombsole }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        process-example = final.haskellPackages.callCabal2nix "process-example" ./. {};
        echopy = prev.callPackage ./echopy/build.nix {};
      });
      packages = forAllSystems (system: {
         process-example = nixpkgsFor.${system}.process-example;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.process-example);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.process-example];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            libzombsole.packages.${system}.default
            nixpkgsFor.${system}.echopy
          ];
        # Change the prompt to show that you are in a devShell
        shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
  };
}
