{ mkDerivation, base, dhall, directory, filepath, HUnit, microlens
, optparse-applicative, parsec, stdenv, tasty, tasty-hunit
, template-haskell, text, time, unix
}:
mkDerivation {
  pname = "dzen-dhall";
  version = "0.0.0.1";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base dhall directory filepath optparse-applicative parsec text time
    unix
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base dhall filepath HUnit microlens parsec tasty tasty-hunit
    template-haskell text
  ];
  homepage = "https://github.com/klntsky/dzen-dhall";
  description = "Configure dzen2 bars in Dhall language";
  license = stdenv.lib.licenses.bsd3;
}
