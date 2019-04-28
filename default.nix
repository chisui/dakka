{ pkgs ? import ./nixpkgs.pinned.nix
, hsPkgs ? import ./haskell.pkgs.nix { inherit pkgs; }
}:
let
  drv = hsPkgs.callCabal2nix "dakka" ./. {};
in
  if pkgs.lib.inNixShell then drv.env else drv

