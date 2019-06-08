{ usePinned ? false }:
let
  release = import ./nix/release.nix { inherit usePinned; };
in
  { inherit (release) dzen-dhall;
  }
