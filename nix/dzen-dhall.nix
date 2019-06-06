{ mkDerivation, async, base, dhall, directory, filepath, hspec
, HUnit, microlens, optparse-applicative, parsec, process, stdenv
, tasty, tasty-hspec, tasty-hunit, template-haskell, text, time
, unix
}:
mkDerivation {
  pname = "dzen-dhall";
  version = "0.0.0.1";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    async base dhall directory filepath optparse-applicative parsec
    process text time unix
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base dhall filepath hspec HUnit microlens optparse-applicative
    parsec tasty tasty-hspec tasty-hunit template-haskell text
  ];
  homepage = "https://github.com/klntsky/dzen-dhall";
  description = "Configure dzen2 bars in Dhall language";
  license = stdenv.lib.licenses.bsd3;
}
