{ nixpkgs ? <nixpkgs>
, pkgs ? import nixpkgs {}
, fetchurl ? pkgs.fetchurl
, mkDerivation ? pkgs.stdenv.mkDerivation
}:
mkDerivation {
  name = "journal-of-computer-information-systems.csl";
  version = "0.1.0";
  src = fetchurl {
    url    = "https://raw.githubusercontent.com/citation-style-language/styles/73a405779d590a45424650c712b43e6417b412c0/journal-of-computer-information-systems.csl";
    sha256 = "803069db5fedba92f478ba8b91a3235cb0b892e1d8edc57b611b832d3f763b95";
  };        
  unpackPhase    = ":";
  buildPhase     = ":";
  installPhase   = "cp $src $out";
  configurePhase = ":";
  fixupPhase     = ":";
}
