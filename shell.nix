{ nixpkgs ? import <nixpkgs> {}, usePinned ? false }:
let drv = (import ./nix/release.nix { inherit usePinned; }).dzen-dhall;
in
  if nixpkgs.lib.inNixShell
  then drv.env
  else drv
