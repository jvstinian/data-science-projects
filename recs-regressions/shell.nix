# nixpkgs is pinned to a revision of nixos-23.11
{
  pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/617579a787259b9a6419492eaac670a5f7663917.tar.gz") {}
}: 
let
  python-packages = ps: with ps; [
    jupyter
    ipython
    pandas
    numpy
    statsmodels
    seaborn
    tabulate # for writing pandas dataframes to markdown
  ];
  python-with-packages = pkgs.python3.withPackages python-packages;
in 
pkgs.mkShell {
  buildInputs = [ python-with-packages ];
  shellHook = ''
      jupyter notebook --no-browser --port=8888
  '';
}
