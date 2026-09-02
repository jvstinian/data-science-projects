{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-25.05";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, ... }: 
    let 
      ale-overlay = final: prev: {
        atari-roms-targz = final.pkgs.stdenv.mkDerivation rec {
            name = "atari-roms-targz";
            version = "1.0.0";
            src = final.pkgs.fetchurl {
              url = "https://gist.githubusercontent.com/jjshoots/61b22aefce4456920ba99f2c36906eda/raw/00046ac3403768bfe45857610a3d333b8e35e026/Roms.tar.gz.b64";
              hash = "sha256-Asp3fBZHanL6NmgKK6ePJMOsMbIVUDNUml83oGUxF94=";
            };
            unpackPhase = ''
              runHook preUnpack
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

        pythonPackagesOverlays = (prev.pythonPackagesOverlays or [ ]) ++ [
            (python-final: python-prev: {
                autorom = python-final.buildPythonPackage rec {
                    name = "autorom";
                    version = "64071fb9d2f4d476ca3089c2866fbc08f4d6dbfa";

                    src = final.pkgs.fetchFromGitHub {
                        owner = "Farama-Foundation";
                        repo = "${name}";
                        rev = "${version}";
                        sha256 = "sha256-fC5OOXAnnP4x4j/IbpG0YdTz5F5pgyY0tumNjyrQ8FM=";
                    };

                    sourceRoot = "${src.name}/packages/AutoROM";
              
                    dependencies = (with final; [ atari-roms-targz ]) ++ (with python-final; [
                        click requests
                    ]);

                    nativeBuildInputs = with python-final; [ click requests ];
                    propagatedBuildInputs = with python-final; [ click requests ];
                    nativeCheckInputs = with python-final; [ click requests ];

                    # build-system = with pkgs.python3.pkgs; [ setuptools pip ];
                    doCheck = true;

                    postInstall = ''
                        echo "NOTE: postInstall - COPYING ROMS"
                        $out/bin/AutoROM -y -s ${final.atari-roms-targz}/Roms.tar.gz -d $out/lib/python3.12/site-packages/AutoROM/roms
                    '';

                    meta = {
                        homepage = "https://github.com/Farama-Foundation/AutoROM";
                        description = "A tool to automate installing Atari ROMs for the Arcade Learning Environment";
                        license = final.pkgs.lib.licenses.mit;
                        maintainers = [ "Farama Foundation" ];
                    };
                };
                autorom-accept-rom-license = python-final.buildPythonPackage rec {
                    name = "autorom-accept-rom-license";
                    version = "64071fb9d2f4d476ca3089c2866fbc08f4d6dbfa";

                    src = final.pkgs.fetchFromGitHub {
                        owner = "Farama-Foundation";
                        repo = "${name}";
                        rev = "${version}";
                        sha256 = "sha256-fC5OOXAnnP4x4j/IbpG0YdTz5F5pgyY0tumNjyrQ8FM=";
                    };

                    sourceRoot = "${src.name}/packages/AutoROM.accept-rom-license";
                      
                    dependencies = with python-final; [ click requests ];
                    # Note autorom in the following
                    nativeBuildInputs = (with python-final; [ click requests autorom ]); 
                    propagatedBuildInputs = with python-final; [ click requests ];
                    nativeCheckInputs = with python-final; [ click requests ];

                    doCheck = true;

                    meta = {
                        homepage = "https://github.com/Farama-Foundation/AutoROM";
                        description = "A tool to automate installing Atari ROMs for the Arcade Learning Environment";
                        license = final.pkgs.lib.licenses.mit;
                        maintainers = [ "Farama Foundation" ];
                    };
                };
                ale-py-with-roms = python-final.ale-py.overrideAttrs (oldAttrs: {
                    # works now, copying roms to autoroms/roms isn't sufficient anymore, copying to ale_py/roms seems to work
                    postInstall = ''
                        echo "NOTE: postInstall - COPYING ROMS - ale-py"
                        ${python-final.autorom}/bin/AutoROM -y -s ${final.atari-roms-targz}/Roms.tar.gz -d $out/lib/python3.12/site-packages/ale_py/roms
                    '';
                });
            })
        ];

        # Trying the simpler approach described in the manual at 
        # https://nixos.org/manual/nixpkgs/unstable/#how-to-override-a-python-package-using-overlays
        python3 = prev.python3.override {
            packageOverrides = prev.lib.composeManyExtensions final.pythonPackagesOverlays;
        };

        python3Packages = final.python3.pkgs;

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

            nativeBuildInputs = with final.pkgs; [ cmake vcpkg pkg-config ];
            buildInputs = with final.pkgs; [ zlib SDL2 vcpkg ];

            # Pass CMake flags
            cmakeFlags = [ "-DCMAKE_BUILD_TYPE=Release" "-DSDL_SUPPORT=ON" "-DBUILD_PYTHON_LIB=OFF" ];
        };
        ale-cpp-pkgconfig = final.pkgs.writeTextFile {
            name = "ale-cpp-pkgconfig";
            text = ''
              # ale.pc
              prefix=${final.ale-cpp}
              includedir=${final.ale-cpp}/include/ale
              libdir=''${prefix}/lib

              Name: ale
              Version: ${final.ale-cpp.version}
              Description: ALE C++ library
              Cflags: -I''${includedir}
              Libs: -L''${libdir} -lale
            '';
            destination = "/lib/pkgconfig/ale.pc";
        };
        ale-c = final.pkgs.stdenv.mkDerivation rec {
            pname = "ale-c";
            version = "0.1.0";

            src = ./.;
 
            nativeBuildInputs = with final.pkgs; [ cmake pkg-config ];
            buildInputs = with final.pkgs; [ zlib SDL2 ale-cpp ale-cpp-pkgconfig openssl check ];

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
              jupyter ipython matplotlib
              ale-py-with-roms
              autorom
              autorom-accept-rom-license
          ];
    
          dev-python = pkgs.python3.withPackages dev-python-packages;
        in 
          rec {
            devShells = {
              default = pkgs.mkShell {
                buildInputs = with pkgs; [
                  dev-python
                  ale-cpp
                  ale-c
                  cmake SDL2 SDL2.dev zlib openssl check
                ];
                nativeBuildInputs = with pkgs; [ pkg-config ale-cpp-pkgconfig ];
                shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
       dev-python
       pkgs.SDL2
       pkgs.zlib
    ]}:$LD_LIBRARY_PATH"
    export PS1="\[\e[1;34m\]ale-c > \[\e[0m\]";
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
