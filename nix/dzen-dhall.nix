{ mkDerivation, async, base, bytestring, dhall, directory, filepath
, hspec, http-conduit, HUnit, megaparsec, microlens, microlens-th
, network-uri, optparse-applicative, parsec, parsers, process
, stdenv, tasty, tasty-hspec, tasty-hunit, template-haskell, text
, time, transformers, unix, unordered-containers, utf8-string
}:
mkDerivation {
  pname = "dzen-dhall";
  version = "0.0.0.1";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    async base bytestring dhall directory filepath http-conduit
    megaparsec microlens microlens-th network-uri optparse-applicative
    parsec parsers process text time transformers unix
    unordered-containers utf8-string
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base dhall filepath hspec HUnit microlens network-uri
    optparse-applicative parsec tasty tasty-hspec tasty-hunit
    template-haskell text
  ];
  homepage = "https://github.com/klntsky/dzen-dhall";
  description = "Configure dzen2 bars in Dhall language";
  license = stdenv.lib.licenses.bsd3;
}
