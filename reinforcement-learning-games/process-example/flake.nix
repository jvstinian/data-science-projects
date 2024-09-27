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
	my-process-bundle = final.symlinkJoin { # TODO: Perhaps change the name
              name = "my-process-bundle";
              # meta.mainProgram = "process-example";
	      buildInputs = [ final.makeWrapper ];
	      postBuild = ''
	          echo "Links added in symlinkJoin"
		  makeWrapper ${final.process-example}/bin/process-example $out/bin/my-wrapper --prefix PATH : $out/bin
	      '';
              paths = with final; [
                final.process-example
                final.echopy
                libzombsole.packages.${final.system}.default # TODO: This seems to work, but is it the way to go?
              ];
        };
      });
      apps = forAllSystems (system: {
        bundle-process = { # TODO: Perhaps change the name
            type = "app";
            program = "${nixpkgsFor.${system}.my-process-bundle}/bin/my-wrapper";
	};
      });
      # Look into the following for wrapping the necessary executables with process-example
      # https://discourse.nixos.org/t/adding-runtime-dependency-to-flake/27785
      packages = forAllSystems (system: {
         process-example = nixpkgsFor.${system}.process-example;
	 my-process-bundle = nixpkgsFor.${system}.my-process-bundle;
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
