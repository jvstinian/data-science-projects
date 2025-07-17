{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/e6f23dc08d3624daab7094b701aa3954923c6bbb";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, ... }: 
    let
      neovim-overlay = final: prev: {
              neovim = prev.neovim.override {
                  configure = {
                      packages.myVimPackage = with prev.pkgs.vimPlugins; {
                          start = [
                              {
                                  plugin = nvim-lspconfig;
                                  type = "lua";
                                  config = ''
                                      " packadd! nvim-lspconfig.lua
                                      lua << END
                                      require'lspconfig'.zls.setup{}
                                      END
                                  '';
                              } 
                              nvim-treesitter
                              ctrlp-vim
                              nvim-treesitter-parsers.cpp
                              copilot-vim
                          ];
                          opt = [ ];
                      }; 
                  };
              };
      };
    in 
      flake-utils.lib.eachDefaultSystem (system:
        let 
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ neovim-overlay ];
          };
    
          id3-zig = pkgs.stdenv.mkDerivation {
            pname = "id3-zig";
            version = "0.1.0";
            src = ./.;
            description = "ID3 algorithm implemented in Zig";

            nativeBuildInputs = [
              pkgs.zig.hook
            ];
          };
      in rec {
        devShell = pkgs.mkShell {
          inputsFrom = with pkgs; [
            zig zls
          ];
          packages = with pkgs; [ neovim zls ];
          shellHook = "export PS1='\\[\\e[1;34m\\]zig-id3-dev > \\[\\e[0m\\]'";
        };
        packages = {
          default = id3-zig;
        };
        apps.default = {
          type = "app";
          program = "${packages.default}/bin/id3";
        };
      }
    ) // {
      overlays.default = neovim-overlay;
    };
}
