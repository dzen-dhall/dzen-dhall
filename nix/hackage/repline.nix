{ mkDerivation, base, containers, exceptions, haskeline, mtl
, process, stdenv
}:
mkDerivation {
  pname = "repline";
  version = "0.2.2.0";
  sha256 = "a191edc3fd5ade0035e17792bf98cdf54eeedc4293b02209da250959806bc519";
  libraryHaskellDepends = [
    base containers exceptions haskeline mtl process
  ];
  homepage = "https://github.com/sdiehl/repline";
  description = "Haskeline wrapper for GHCi-like REPL interfaces";
  license = stdenv.lib.licenses.mit;
}
