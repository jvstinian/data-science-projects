{ pkgs ? import <nixpkgs> {} }:
(
  let base = pkgs.appimageTools.defaultFhsEnvArgs; in
  pkgs.buildFHSEnv (base // {
    name = "FHS";
    targetPkgs = pkgs: (with pkgs; [
      gnat alire gprbuild unzip # valgrind
      gcc glibc zstd # zlib binutils
    ]);
    runScript = "bash";
    extraOutputsToInstall = [ "dev" ];
    # profile = ''
    #   export PS1="FHS $PS1"
    # '';
  })
).env
