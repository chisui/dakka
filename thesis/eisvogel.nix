{ pkgs
, texlive ? pkgs.texlive
, mkDerivation ? pkgs.stdenv.mkDerivation
, fetchurl ? pkgs.fetchurl
}:
mkDerivation {
  name    = "eisvogel.latex";
  version = "0.0.1";

  src = ./eisvogel.tex;

  phases = ["installPhase"];
  installPhase = "cp $src $out";
} // {
  texlivePackages = {
    inherit (texlive)
      adjustbox
      ucs
      collectbox
      collection-fontsrecommended
      pagecolor
      koma-script
      csquotes
      mdframed
      needspace
      sourcesanspro
      ly1
      mweights
      sourcecodepro
      titling
      lm
      listings
      float
      xcolor
      setspace
      etoolbox
      caption
      l3packages
      l3kernel
      xkeyval;
  };
}

