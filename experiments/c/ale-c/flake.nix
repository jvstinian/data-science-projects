{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-25.11";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, ... }: 
    let 
      ale-overlay = final: prev: {
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
        ale-py-with-roms = pkgs.python3.pkgs.ale-py.overrideAttrs (oldAttrs: {
          postInstall = ''
            echo "NOTE: postInstall - COPYING ROMS - local ale-py"
            ${autorom}/bin/AutoROM -y -s ${atari-roms-targz}/Roms.tar.gz -d $out/lib/python3.12/site-packages/ale_py/roms
          '';
        });
        ale-cpp = final.pkgs.stdenv.mkDerivation rec {
            pname = "ale-cpp";
            version = "0.11.2";

            src = final.pkgs.fetchFromGitHub {
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
        ale-cpp-pkgconfig = final.pkgs.writeTextFile {
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
        ale-c = pkgs.stdenv.mkDerivation rec {
            pname = "ale-c";
            version = "0.1.0";

            src = ./.;
          
            nativeBuildInputs = with pkgs; [ cmake pkg-config ];
            buildInputs = with pkgs; [ zlib SDL2 ];

            cmakeFlags = [ "-DCMAKE_BUILD_TYPE=Release" ];
        };
      };
    in 
      flake-utils.lib.eachDefaultSystem (system: 
        let 
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ ale-overlay ];
          };

          dev-python-packages = ps: with ps; [
              numpy 
              gymnasium
              pygame
              pybind11 pysdl2
              ale-py-with-roms # works now, copying roms to autoroms/roms isn't sufficient anymore, copying to ale_py/roms seems to work
              autorom
              autorom-accept-rom-license
              jupyter ipython matplotlib # TODO: Are these needed?
          ];
    
          dev-python = pkgs.python3.withPackages dev-python-packages;
        in 
          rec {
            devShells = {
              default = pkgs.mkShell {
                buildInputs = with pkgs; [
                  dev-python
                  ale-cpp
                  cmake SDL2 SDL2.dev zlib openssl check
                ];
                nativeBuildInputs = [ ale-cpp-pkgconfig ] ++ (with pkgs; [ pkg-config ]);
                shellHook = ''
    # This does work, `python` can be used instead of `my-wrapper`
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
       dev-python
       pkgs.SDL2
       pkgs.zlib
    ]}:$LD_LIBRARY_PATH"
    export PS1='\\[\\e[1;34m\\]ale-c > \\[\\e[0m\\]';
                '';
              };
            };
            packages = {
              ale-c = pkgs.ale-c;
              default = pkgs.ale-c;
            };
            apps.default = {
              type = "app";
              program = "${packages.ale-c}/bin/ale-example";
            };
          }
      ) // {
        overlays.default = ale-overlay;
      };
}
