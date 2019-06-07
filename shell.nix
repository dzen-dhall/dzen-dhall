{ nixpkgs ? import <nixpkgs> {} }:
let drv = (import ./nix/release.nix).dzen-dhall;
in
  if nixpkgs.lib.inNixShell
  then drv.env
  else drv
