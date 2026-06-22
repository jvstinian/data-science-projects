{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-25.05";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      hcdg = pkgs.stdenv.mkDerivation rec {
          name = "hcdg";
          version = "0.1.0";
          src = ./.;
          nativeBuildInputs = with pkgs; [ pkg-config autoreconfHook ];
          # buildInputs = with pkgs; [
          # ];
          # installPhase = ''
          #   mkdir -p $out/bin
          #   cp src/line_walk $out/bin/line_walk
          # '';
          # configureFlags = with pkgs; [
          #   "--with-mysql=${mariadb-connector-c.dev}"
          # ];
      };

      overlay = final: prev: {
        line_walk = line_walk;
      };

      pkgs = import nixpkgs {
        inherit system;
        overlays = [ overlay ];
      };

    in rec {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          pkg-config
          automake autoconf libtool check
          clang-tools
          ctags cscope
        ];
      };
      packages = {
        default = pkgs.hcdg;
      };
      apps.default = {
        type = "app";
        program = "${packages.default}/bin/line_walk";
      };
    }
  );
}
