{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;
  f = { mkDerivation, base, mtl, stdenv, transformers, containers, contravariant, distributed-process, distributed-static, network-transport-tcp, should-not-typecheck, tasty, tasty-quickcheck, tasty-hunit, freer-simple }:
      mkDerivation {
        pname = "dakka";
        version = "0.0.1";
        src = ./.;
        libraryHaskellDepends = [ base transformers mtl contravariant containers distributed-process distributed-static network-transport-tcp should-not-typecheck tasty tasty-quickcheck tasty-hunit freer-simple ];
        description = "dakka";                                                                                                                   
        license = stdenv.lib.licenses.mit;                                                                                                       
      };

  drv = pkgs.haskell.packages.ghc842.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv

