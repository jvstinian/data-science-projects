{ lib, mkDerivation, 
  # stdenv, 
  ad, base, vector, statistics, math-functions }:
mkDerivation {
    pname = "statistical-models";
    version = "0.0.0.1";
    src = ./.;
    libraryHaskellDepends = [ ad base vector math-functions statistics ];
    description = "Statistical models";
    license = lib.licenses.bsd3;
    hydraPlatforms = lib.platforms.none;
    broken = false;
}
