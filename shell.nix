{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;
  f = { mkDerivation, base, singletons, stdenv, transformers }:
      mkDerivation {
        pname = "myPkg";
        version = "0.0.1";
        src = ./.;
        libraryHaskellDepends = [ base singletons transformers ];
        description = "myPkg";
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.ghc841.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv

