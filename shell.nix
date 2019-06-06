{ nixpkgs ? import <nixpkgs> {} }:
let drv = (import ./nix/release.nix).dzen-dhall.env;
in
  if pkgs.lib.inNixShell
  then drv.env
  else drv
