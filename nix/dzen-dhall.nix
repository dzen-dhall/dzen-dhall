{ mkDerivation, base, dhall, directory, filepath, HUnit, microlens
, optparse-applicative, stdenv, tasty, tasty-hunit
, template-haskell, text, unix
}:
mkDerivation {
  pname = "dzen-dhall";
  version = "0.0.0.1";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base dhall directory filepath optparse-applicative text unix
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base dhall filepath HUnit microlens tasty tasty-hunit
    template-haskell text
  ];
  homepage = "https://github.com/klntsky/dzen-dhall";
  description = "Configure dzen2 bars in Dhall language";
  license = stdenv.lib.licenses.bsd3;
}
