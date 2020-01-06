{ mkDerivation, aeson, aeson-pretty, ansi-terminal, atomic-write
, base, bytestring, case-insensitive, cborg, cborg-json, containers
, contravariant, cryptonite, data-fix, deepseq, Diff, directory
, doctest, dotgen, either, exceptions, filepath, foldl, gauge
, generic-random, hashable, haskeline, http-client, http-client-tls
, http-types, lens-family-core, megaparsec, memory, mockery, mtl
, network-uri, optparse-applicative, parsers, pretty-simple
, prettyprinter, prettyprinter-ansi-terminal, profunctors
, QuickCheck, quickcheck-instances, repline, scientific, semigroups
, serialise, special-values, spoon, stdenv, tasty
, tasty-expected-failure, tasty-hunit, tasty-quickcheck
, template-haskell, text, th-lift-instances, transformers
, transformers-compat, turtle, unordered-containers, uri-encode
, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.28.0";
  sha256 = "41b089d480f45548d612d07577361a1086bb41da17204dc090c89c1a93423c4e";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-terminal atomic-write base bytestring
    case-insensitive cborg cborg-json containers contravariant
    cryptonite data-fix deepseq Diff directory dotgen either exceptions
    filepath hashable haskeline http-client http-client-tls http-types
    lens-family-core megaparsec memory mtl network-uri
    optparse-applicative parsers pretty-simple prettyprinter
    prettyprinter-ansi-terminal profunctors repline scientific
    serialise template-haskell text th-lift-instances transformers
    transformers-compat unordered-containers uri-encode vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring cborg containers data-fix deepseq directory doctest
    either filepath foldl generic-random lens-family-core megaparsec
    mockery prettyprinter QuickCheck quickcheck-instances scientific
    semigroups serialise special-values spoon tasty
    tasty-expected-failure tasty-hunit tasty-quickcheck text
    transformers turtle unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    base bytestring containers directory gauge serialise text
  ];
  doHaddock = false;
  doCheck = false;
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
