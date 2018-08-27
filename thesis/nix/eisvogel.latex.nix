{ nixpkgs ? <nixpkgs> 
, pkgs ? import nixpkgs {}
, texlive ? pkgs.texlive
, mkSingleFile ? import ./mkSingleFile.nix { inherit pkgs; }
, fetchurl ? pkgs.fetchurl
}:
mkSingleFile {
  name    = "eisvogel.latex";
  version = "0.0.1";
  src = fetchurl {
    url    = "https://raw.githubusercontent.com/Wandmalfarbe/pandoc-latex-template/540173c741da6cc466fb698bc49d78d4f196c1a9/eisvogel.tex";
    sha256 = "c81d55720e62d40963d33ea41f10def78059fb5f9d7359a33a2cf3db2411a6dd";
  };
} // {
  texlivePackages = {
    inherit (texlive)
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

