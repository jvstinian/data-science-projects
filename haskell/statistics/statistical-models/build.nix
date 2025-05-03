{ lib, mkDerivation, 
  # stdenv, 
  ad, base, vector, statistics, math-functions }:
mkDerivation {
    pname = "statistical-models";
    version = "0.0.0.1";
    src = ./.;
    # sha256 = "00b4n4gw5y0mpayb0zlkvz91nfrpbspz22kqhpvdnxbb4zcz7pnj";
    libraryHaskellDepends = [ ad base vector math-functions statistics ];
    description = "Statistical models";
    license = lib.licenses.bsd2; # TODO
    hydraPlatforms = lib.platforms.none;
    broken = false;
}
