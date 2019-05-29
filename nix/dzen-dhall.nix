{ mkDerivation, base, dhall, directory, optparse-applicative
, stdenv, unix
}:
mkDerivation {
  pname = "dzen-dhall";
  version = "0.0.0.1";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base dhall ];
  executableHaskellDepends = [
    base dhall directory optparse-applicative unix
  ];
  homepage = "https://github.com/klntsky/dzen-dhall";
  description = "Configure dzen2 bars in Dhall language";
  license = stdenv.lib.licenses.bsd3;
}
