{ nixpkgs ? <nixpkgs> 
, pkgs ? import nixpkgs {}
, mkDerivation ? pkgs.stdenv.mkDerivation
}:
{ name, version, src }:
mkDerivation {
  inherit name version src;
  unpackPhase    = ":";
  buildPhase     = ":";
  installPhase   = "cp $src $out";
  configurePhase = ":";
  fixupPhase     = ":";
}
