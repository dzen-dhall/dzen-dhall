{ mkDerivation, base, ghc-prim, stdenv, template-haskell
, th-abstraction
}:
mkDerivation {
  pname = "th-lift";
  version = "0.8.1";
  sha256 = "3fa1f4193794d8d6dc7864e20a2f89ab268c321a4b3d254fd38282619f8e5ed7";
  libraryHaskellDepends = [
    base ghc-prim template-haskell th-abstraction
  ];
  testHaskellDepends = [ base ghc-prim template-haskell ];
  doHaddock = false;
  doCheck = false;
  homepage = "http://github.com/RyanGlScott/th-lift";
  description = "Derive Template Haskell's Lift class for datatypes";
  license = stdenv.lib.licenses.bsd3;
}
