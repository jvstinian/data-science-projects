{
  pkgs ? import <nixpkgs> {}
  # pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/ea5234e7073d5f44728c499192544a84244bf35a.tar.gz") {}
}: 
let
  # https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/python.section.md
  jvstinian-zombsole = pkgs.python310Packages.buildPythonPackage rec {
      name = "zombsole";
      version = "8f52601d4f5669e5667e94f51e0c972863653bd6"; 
      # "373c59d1d5badbe43ce560eef0d9595b3d0bf3fc"; 
      # "2f13126771ccfe73e30801524e123b089d452a11";
      # version = "0.3.1"; # or "v0.3.1"?

      src = pkgs.fetchFromGitHub {
      	 owner = "jvstinian";
	 repo = "${name}";
	 rev = "${version}";
	 sha256 = "iLWCDc8ht/t5uPEo04HUsKTE13RlxskgXxgym/Nn3vM=";
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
  my-python-packages = ps: with ps; [
    docopt
    termcolor
    # pandas
    # tensorflow
    # opencv4
    # pygame
    jvstinian-zombsole
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
      zombsole extermination  me,terminator:2,sniper:2,hamster -m bridge  -z 50 -n 0 -b; exit 
  '';
}

