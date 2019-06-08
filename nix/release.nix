{ usePinned ? false }:
let
  bootstrap = import <nixpkgs> { };

  pinnedNixpkgsRepo = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  pinnedNixpkgs = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (pinnedNixpkgsRepo) rev sha256;
  };

  pkgs =
    if usePinned
    then import pinnedNixpkgs { inherit config; }
    else import <nixpkgs>     { inherit config; };

  # See https://github.com/Gabriel439/haskell-nix/blob/master/project1/README.md#changing-versions
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          dzen-dhall = haskellPackagesNew.callPackage ./dzen-dhall.nix { };

          # Direct dependencies
          dhall = haskellPackagesNew.callPackage ./hackage/dhall.nix { };

          # Indirect dependencies
          repline = haskellPackagesNew.callPackage ./hackage/repline.nix { };

        };
      };
    };
  };

in
{
  dzen-dhall = pkgs.haskellPackages.dzen-dhall;
}
