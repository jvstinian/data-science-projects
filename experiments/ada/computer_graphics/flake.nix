{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/1c8ba8d3f7634acac4a2094eef7c32ad9106532c"; 
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
      in rec {
        devShell = pkgs.mkShell {
          inputsFrom = with pkgs; [
            SDL2 gnat
          ];
          packages = with pkgs; [ neovim ];
          shellHook = "export PS1='\\[\\e[1;34m\\]ada-dev > \\[\\e[0m\\]'";
        };
        # packages = {
        #   default = id3-zig;
        # };
        # apps.default = {
        #   type = "app";
        #   program = "${packages.default}/bin/id3";
        # };
      }
    ) // {
      overlays.default = neovim-overlay;
    };
}
