{ pkgs ? import ../nixpkgs.pinned.nix }: 
let
  drv = pkgs.haskellPackages.callCabal2nix "dakka" ./. {};
in
  if pkgs.lib.inNixShell then drv.env else drv

