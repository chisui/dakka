{ nixpkgsVersion ? builtins.fromJSON (builtins.readFile ./nixpkgs-version.json)
, nixpkgs ? builtins.fetchTarball {
    inherit (nixpkgsVersion) sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
  }
, pkgs ? import nixpkgs {}
}:

let
  drv = pkgs.haskell.packages.ghc843.callPackage (import ./dakka.nix) {};
in
  if pkgs.lib.inNixShell then drv.env else drv

