{
  # pkgs ? import <nixpkgs> {}
  # pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/ea5234e7073d5f44728c499192544a84244bf35a.tar.gz") {}
  # pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/394571358ce82dff7411395829aa6a3aad45b907.tar.gz") {}
  # pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/72d11d40b9878a67c38f003c240c2d2e1811e72a.tar.gz") {} # 24.05 I think
  pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/d3c42f187194c26d9f0309a8ecc469d6c878ce33.tar.gz") {} # unstable
}: 
let
  # https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/python.section.md
  atari-roms-targz = pkgs.stdenv.mkDerivation rec {
    name = "atari-roms-targz";
    version = "1.0.0";
    src = pkgs.fetchurl {
      url = "https://gist.githubusercontent.com/jjshoots/61b22aefce4456920ba99f2c36906eda/raw/00046ac3403768bfe45857610a3d333b8e35e026/Roms.tar.gz.b64";
      hash = "sha256-Asp3fBZHanL6NmgKK6ePJMOsMbIVUDNUml83oGUxF94=";
    };
    unpackPhase = ''
      runHook preUnpack
      echo "LS: $(ls)"
      echo "SRC: $src"
      echo "OUT: $(basename $(stripHash "$src") .b64)"
      # cp "$src" $(stripHash "$_src")
      base64 -d ${src} > $(basename $(stripHash "$src") .b64)
      runHook postUnpack
    '';
    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp Roms.tar.gz $out/
      runHook postInstall
    '';
  };
  atari-roms = pkgs.stdenv.mkDerivation rec { # TODO: I don't think this is needed anymore
    name = "atari-roms";
    version = "1.0.0";
    src = pkgs.fetchurl {
      url = "https://gist.githubusercontent.com/jjshoots/61b22aefce4456920ba99f2c36906eda/raw/00046ac3403768bfe45857610a3d333b8e35e026/Roms.tar.gz.b64";
      hash = "sha256-Asp3fBZHanL6NmgKK6ePJMOsMbIVUDNUml83oGUxF94=";
      # downloadToTemp = true;
      # postFetch = ''
      #   base64 -d $downloadedFile > $out
      # '';
    };
    unpackPhase = ''
      runHook preUnpack
      echo "LS: $(ls)"
      echo "SRC: $src"
      echo "OUT: $(basename $(stripHash "$src") .tar.gz.b64)"
      # cp "$src" $(stripHash "$_src")
      base64 -d ${src} | tar xvz # -C $(basename $(stripHash "$src") .tar.gz.b64)
      runHook postUnpack
    '';
    installPhase = ''
      runHook preInstall
      mkdir -p $out/roms
      find ./ -name "*.bin" -exec cp {} $out/roms/ \;
      runHook postInstall
    '';
  };
  autorom = pkgs.python3.pkgs.buildPythonPackage rec {
      name = "autorom";
      version = "64071fb9d2f4d476ca3089c2866fbc08f4d6dbfa";

      src = pkgs.fetchFromGitHub {
      	 owner = "Farama-Foundation";
	 repo = "${name}";
	 rev = "${version}";
	 sha256 = "sha256-fC5OOXAnnP4x4j/IbpG0YdTz5F5pgyY0tumNjyrQ8FM=";
      };

      # sourceRoot = "${src.name}/packages/AutoROM.accept-rom-license";
      sourceRoot = "${src.name}/packages/AutoROM"; # .accept-rom-license";
      
      dependencies = with pkgs.python3.pkgs; [
        # atari-roms  # TODO: I don't think this is needed
        atari-roms-targz click requests
      ];

      nativeBuildInputs = with pkgs.python3.pkgs; [
        click requests
      ];

      propagatedBuildInputs = with pkgs.python3.pkgs; [
        click requests
      ];

      nativeCheckInputs = with pkgs.python3.pkgs; [
        click requests
      ];

      # buildInputs = with pkgs.python3.pkgs; [ docopt ];

      # build-system = with pkgs.python3.pkgs; [ setuptools pip ];
      # propagatedBuildInputs = with pkgs.python3Packages; [ setproctitle ];
      doCheck = true; # tests failing

      # optional-dependencies = {
      #   accept-rom-license = with pkgs.python3.pkgs; [ farama-notifications ];
      # };
      # postInstall = ''
      #   echo "NOTE: postInstall"
      #   ls $out/lib/python3.11/site-packages/
      #   $out/bin/AutoROM -y
      # '';
      postInstall = ''
        echo "NOTE: postInstall - COPYING ROMS"
        # ls ${atari-roms}/
        # The following works too
        # cp -v ${atari-roms}/roms/* $out/lib/python3.11/site-packages/AutoROM/roms/
        # For python 3.11: 
        # $out/bin/AutoROM -y -s ${atari-roms-targz}/Roms.tar.gz -d $out/lib/python3.11/site-packages/AutoROM/roms
        $out/bin/AutoROM -y -s ${atari-roms-targz}/Roms.tar.gz -d $out/lib/python3.12/site-packages/AutoROM/roms
      '';
      # installPhase = ''
      #   runHook preInstall
      #   echo "In autorom, running install phase"
      #   ls ./
      #   ls ./dist/
      #   runHook installPhase
      #   runHook posInstall
      #   echo "NOTE: Attempting to install roms"
      #   $out/bin/AutoROM -y;
      # '';

      meta = {
        homepage = "https://github.com/Farama-Foundation/AutoROM";
        description = "Description here.";
        license = pkgs.lib.licenses.mit;
        maintainers = [ "Farama Foundation" ];
      };
    };
  autorom-accept-rom-license = pkgs.python3.pkgs.buildPythonPackage rec {
      name = "autorom-accept-rom-license";
      version = "64071fb9d2f4d476ca3089c2866fbc08f4d6dbfa";

      src = pkgs.fetchFromGitHub {
      	 owner = "Farama-Foundation";
	 repo = "${name}";
	 rev = "${version}";
	 sha256 = "sha256-fC5OOXAnnP4x4j/IbpG0YdTz5F5pgyY0tumNjyrQ8FM=";
      };

      sourceRoot = "${src.name}/packages/AutoROM.accept-rom-license";
      # sourceRoot = "${src.name}/packages/AutoROM"; # .accept-rom-license";
      
      dependencies = with pkgs.python3.pkgs; [
        click requests
      ];

      nativeBuildInputs = (with pkgs.python3.pkgs; [
        click requests
      ]) ++ [ autorom ];

      propagatedBuildInputs = with pkgs.python3.pkgs; [
        click requests
      ];

      nativeCheckInputs = with pkgs.python3.pkgs; [
        click requests
      ];

      # buildInputs = with pkgs.python3.pkgs; [ docopt ];

      # build-system = with pkgs.python3.pkgs; [ setuptools pip ];
      # propagatedBuildInputs = with pkgs.python3Packages; [ setproctitle ];
      doCheck = true; # tests failing

      # optional-dependencies = {
      #   accept-rom-license = with pkgs.python3.pkgs; [ farama-notifications ];
      # };
      # postInstall = "$out/bin/AutoROM -y; ls $out/lib/python3.11/site-packages/";

      meta = {
        homepage = "https://github.com/Farama-Foundation/AutoROM";
        description = "Description here.";
        license = pkgs.lib.licenses.mit;
        maintainers = [ "Farama Foundation" ];
      };
    };
    # NOTE: I don't think the following makes a difference
    ale-py-with-roms = pkgs.python3.pkgs.ale-py.overrideAttrs (oldAttrs: {
      # cmakeFlags = [
      #   "-DSDL_SUPPORT=ON"
      # ];
      # Turn on SDL support
      # TODO
      # postPatch = oldAttrs.postPatch + ''
      #   substituteInPlace CMakeLists.txt \
      #     --replace-fail \
      #       'option(SDL_SUPPORT "Enable SDL support" OFF)' \
      #       'option(SDL_SUPPORT "Enable SDL support" ON)'
      # '' + ''
      #   substituteInPlace src/ale/CMakeLists.txt \
      #     --replace-fail \
      #       'option(SDL_DYNLOAD "Dynamically load SDL" OFF)' \
      #       'option(SDL_DYNLOAD "Dynamically load SDL" ON)'
      # '';
      postInstall = ''
        echo "NOTE: postInstall - COPYING ROMS - local ale-py"
        ${autorom}/bin/AutoROM -y -s ${atari-roms-targz}/Roms.tar.gz -d $out/lib/python3.12/site-packages/ale_py/roms
      '';
    });
    ale-cpp = pkgs.stdenv.mkDerivation rec {
        pname = "ale-cpp";
        version = "0.11.2";

        src = pkgs.fetchFromGitHub {
      	    owner = "Farama-Foundation";
            repo = "Arcade-Learning-Environment";
            rev = "v${version}";
            sha256 = "sha256-4IkjW8HX21uBEHFtb3qETxco6FfDMgLbG1BDHWwvn58=";
        };
      
        postPatch = ''
            substituteInPlace src/ale/CMakeLists.txt \
                --replace-fail \
                'set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)' \
                'set(CMAKE_INTERPROCEDURAL_OPTIMIZATION FALSE)'
        '';

        # build-system = with pkgs; [ cmake ninja ];

        nativeBuildInputs = with pkgs; [ cmake vcpkg pkg-config ];

        buildInputs = with pkgs; [ zlib SDL2 vcpkg ];

        # dontUseCmakeConfigure = true;

        # Optional: You can pass specific CMake flags if needed
        cmakeFlags = [ "-DCMAKE_BUILD_TYPE=Release" "-DSDL_SUPPORT=ON" "-DBUILD_PYTHON_LIB=OFF" ];
    };
    ale-cpp-pkgconfig = pkgs.writeTextFile {
        name = "ale-cpp-pkgconfig";
        text = ''
          # ale.pc
          prefix=${ale-cpp}
          includedir=${ale-cpp}/include/ale
          libdir=''${prefix}/lib
  
          Name: ale
          Version: ${ale-cpp.version}
          Description: ALE C++ library
          Cflags: -I''${includedir}
          Libs: -L''${libdir} -lale
          # Libs.private: -lz -lm
        '';
        destination = "/lib/pkgconfig/ale.pc";
      };
    # ale-cpp-variant2 = pkgs.stdenv.mkDerivation rec {
    #     pname = "ale-cpp";
    #     version = "0.11.2";

    #     src = pkgs.fetchFromGitHub {
    #   	    owner = "Farama-Foundation";
    #         repo = "Arcade-Learning-Environment";
    #         rev = "v${version}";
    #         sha256 = "sha256-4IkjW8HX21uBEHFtb3qETxco6FfDMgLbG1BDHWwvn58=";
    #     };
    #    
    #     patches = [ ./ale-cpp.patch ]; 

    #     # build-system = with pkgs; [ cmake ninja ];

    #     nativeBuildInputs = with pkgs; [ cmake vcpkg pkg-config ];

    #     buildInputs = with pkgs; [ zlib SDL2 vcpkg ];

    #     # dontUseCmakeConfigure = true;

    #     # Optional: You can pass specific CMake flags if needed
    #     cmakeFlags = [ "-DCMAKE_BUILD_TYPE=Release" "-DSDL_SUPPORT=ON" "-DBUILD_PYTHON_LIB=OFF" "-DINTERPROCEDURAL_OPTIMIZATION_SUPPORT=OFF" "-DBUILD_SHARED_LIBS=ON" ];
    # };
    # ale-cpp-variant2-pkgconfig = pkgs.writeTextFile {
    #     name = "ale-cpp-pkgconfig";
    #     text = ''
    #       # ale.pc
    #       prefix=${ale-cpp-variant2}
    #       includedir=${ale-cpp-variant2}/include/ale
    #       libdir=''${prefix}/lib
  
    #       Name: ale
    #       Version: ${ale-cpp-variant2.version}
    #       Description: ALE C++ library
    #       Cflags: -I''${includedir}
    #       Libs: -L''${libdir} -lale
    #       # Libs.private: -lz -lm
    #     '';
    #     destination = "/lib/pkgconfig/ale.pc";
    #   };
  my-python-packages = ps: with ps; [
    gymnasium
    pygame 
    pybind11 pysdl2
    ale-py-with-roms # works now, copying roms to autoroms/roms isn't sufficient anymore, copying to ale_py/roms seems to work
    autorom
    autorom-accept-rom-license
    mujoco imageio # imageio needed for mujoco
    python-lsp-server
    jupyter ipython matplotlib
  ];
  my-python = pkgs.python3.withPackages my-python-packages;
  my-ghc = (pkgs.haskellPackages.ghcWithPackages (hs: [hs.cabal-install hs.containers hs.inline-c hs.inline-c-cpp hs.splitmix hs.random hs.haskell-language-server]));
in 
# my-python.env
pkgs.mkShell {
  packages = [ my-ghc ale-cpp my-python ] ++ (with pkgs; [ myNeovim SDL2 SDL2.dev zlib ]);
  nativeBuildInputs = [ ale-cpp-pkgconfig ] ++ (with pkgs; [ pkg-config ]);
  # buildInputs = [
  #     my-python pkgs.SDL2 pkgs.SDL2.dev pkgs.zlib
  # ];
  inputsFrom = [
      pkgs.SDL2 my-python
  ];
  shellHook = ''
    # This does work, `python` can be used instead of `my-wrapper`
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
          my-python
          pkgs.SDL2
          pkgs.zlib
        ]}:$LD_LIBRARY_PATH"
  '';
}

