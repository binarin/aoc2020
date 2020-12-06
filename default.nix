{ mkDerivation, base, bytestring, containers, lens, parsec
, parsec-numbers, stdenv
}:
mkDerivation {
  pname = "aoc2020";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers lens parsec parsec-numbers
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
