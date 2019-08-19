{ mkDerivation, ansi-terminal, base, bytestring, containers, dhall
, directory, filepath, generic-random, hashable, hpack, hspec
, http-conduit, HUnit, megaparsec, microlens, microlens-th
, network-uri, optparse-applicative, parsec, parsers, pipes
, prettyprinter, prettyprinter-ansi-terminal, process, QuickCheck
, random, stdenv, tasty, tasty-hspec, tasty-hunit, tasty-quickcheck
, template-haskell, text, time, transformers, unix
, unordered-containers, utf8-string, vector
}:
mkDerivation {
  pname = "dzen-dhall";
  version = "0.0.0.1";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring containers dhall directory filepath
    hashable http-conduit megaparsec microlens microlens-th network-uri
    optparse-applicative parsec parsers pipes prettyprinter
    prettyprinter-ansi-terminal process random text time transformers
    unix unordered-containers utf8-string vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base dhall filepath generic-random hspec HUnit microlens
    network-uri optparse-applicative parsec QuickCheck tasty
    tasty-hspec tasty-hunit tasty-quickcheck template-haskell text
    unordered-containers vector
  ];
  doHaddock = false;
  preConfigure = "hpack";
  description = "Configure dzen2 bars in Dhall language";
  license = stdenv.lib.licenses.bsd3;
}
