let
  pkgs = import <nixpkgs> { 
    overlays = [ 
      (final: prev: {
        opencv4 = prev.opencv4.override { enableFfmpeg = true; enableGtk2 = true; enableGtk3 = true; };
      })
    ];
  }; # pin the channel to ensure reproducibility!
in
pkgs.mkShell {
    name = "opencv-dev";
    packages = with pkgs; [gcc cmake ffmpeg gtk2 gtk3 opencv4];
}
