{
  pkgs ? import <nixpkgs> {}
  # pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/ea5234e7073d5f44728c499192544a84244bf35a.tar.gz") {}
}: 
let
  # https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/python.section.md
  jvstinian-zombsole = pkgs.python310Packages.buildPythonPackage rec {
      name = "zombsole";
      version = "0796721c36e3133e00d7f4c7fb553be33b8556c0";
      # version = "552c037cc47f4d51c5f11ba6a4702edef1675351"; 
      # version = "8f52601d4f5669e5667e94f51e0c972863653bd6"; 
      # "373c59d1d5badbe43ce560eef0d9595b3d0bf3fc"; 
      # "2f13126771ccfe73e30801524e123b089d452a11";
      # version = "0.3.1"; # or "v0.3.1"?

      src = pkgs.fetchFromGitHub {
      	 owner = "jvstinian";
	 repo = "${name}";
	 rev = "${version}";
	 sha256 = "RehO2tDY8nqlSxByCYpfditWyfJ7cnmZUhUGaE6oWeM=";
	 # sha256 = "XEESL8Gh9YJHRGrNr5lFlKq3O/0skHjOuXa5yVe71YE=";
	 # sha256 = "iLWCDc8ht/t5uPEo04HUsKTE13RlxskgXxgym/Nn3vM=";
	 # sha256 = "M5apjkXRg2PxwqPq/N4wlw61Hl9eptKr8mBwrmLPN+s=";
         # sha256 = "FGhWwdgI/ZCXQeljfqmoG1kTF/q/S9OfApDrpX/O8wU=";
      };
      
      dependencies = with pkgs.python310.pkgs; [
        docopt termcolor 
      ];

      # nativeBuildInputs = with pkgs.python310.pkgs; [
      #   docopt termcolor 
      # ];

      nativeCheckInputs = with pkgs.python310.pkgs; [
        requests gym flask docopt termcolor # is pip required?
      ];

      # buildInputs = with pkgs.python310.pkgs; [ docopt ];

      # build-system = with pkgs.python310.pkgs; [ setuptools pip ];
      # propagatedBuildInputs = with pkgs.python3Packages; [ setproctitle ];
      doCheck = true; # tests failing

      meta = {
        homepage = "https://github.com/jvstinian/zombsole";
        description = "Description here.";
        license = pkgs.lib.licenses.mit;
        maintainers = [ "jvstinian" ];
      };
    };
  jvstinian-zombpyg = pkgs.python310Packages.buildPythonPackage rec {
      name = "zombpyg";
      version = "master"; # d14eab08527ccb2b0dec24494276d598f509a880";

      src = pkgs.fetchFromGitHub {
      	 owner = "jvstinian";
	 repo = "${name}";
	 rev = "${version}";
	 sha256 = "DI+WL6GhEqTGV+l8A9I85yOpzs5IKt4XO7D3JYCuZYw=";
      };
      
      dependencies = with pkgs.python310.pkgs; [
        numpy gym pygame
      ];

      # nativeBuildInputs = with pkgs.python310.pkgs; [
      #   docopt termcolor 
      # ];

      nativeCheckInputs = with pkgs.python310.pkgs; [
        numpy gym pygame # pip # requests gym flask docopt termcolor # is pip required?
      ];

      # buildInputs = with pkgs.python310.pkgs; [ docopt ];

      # build-system = with pkgs.python310.pkgs; [ setuptools pip ];
      # propagatedBuildInputs = with pkgs.python3Packages; [ setproctitle ];
      doCheck = true; # tests failing

      meta = {
        homepage = "https://github.com/jvstinian/zombpyg";
        description = "Description here.";
        license = pkgs.lib.licenses.mit;
        maintainers = [ "jvstinian" ];
      };
    };
  my-python-packages = ps: with ps; [
    docopt
    termcolor
    numpy
    tensorflowWithCuda
    opencv4
    pygame # for demo
    gym
    jvstinian-zombsole
    jvstinian-zombpyg
  ];
  # my-python = jvstinian-zombsole.dependencies ++ (pkgs.python310.withPackages my-python-packages);
  my-python = pkgs.python310.withPackages my-python-packages;
in 
# my-python.env
pkgs.mkShell {
  buildInputs = [
      my-python
  ];
  shellHook = ''
      python train_dqn.py -d gpu --config zombsole_cnn > output_cnn_3zombies1min.v3.txt 2>&1
      # python train_dqn.py -d gpu --config demo_mlp
      # python train_dqn.py -d gpu --config zombpyg_mlp
      exit
  '';
}

