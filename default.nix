{ pkgs ? import ./nixpkgs.pinned.nix }:
let
  drv = pkgs.haskell.packages.ghc843.callPackage (import ./dakka.nix) {};
in
  if pkgs.lib.inNixShell then drv.env else drv

