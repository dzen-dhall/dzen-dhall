let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  # See https://github.com/Gabriel439/haskell-nix/blob/master/project1/README.md#changing-versions
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {

          # Direct dependencies
          dhall = haskellPackagesNew.callPackage ./hackage/dhall.nix { };

          # Indirect dependencies
          repline = haskellPackagesNew.callPackage ./hackage/repline.nix { };

        };
      };
    };
  };

  pkgs = import src { inherit config; };

in
{
  dzen-dhall = pkgs.haskellPackages.callPackage ./dzen-dhall.nix { };
}
