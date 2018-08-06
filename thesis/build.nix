{ book ? false }:

with import <nixpkgs> {};

(import ./thesis.nix) {
  mkDerivation = stdenv.mkDerivation;

  texlive = texlive.combine {
    inherit (texlive) scheme-basic collection-fontsrecommended pagecolor koma-script csquotes mdframed needspace sourcesanspro ly1 mweights sourcecodepro titling lm listings float xcolor setspace etoolbox caption l3packages l3kernel xkeyval;
  };

  pandoc = pandoc;
  pandoc-citeproc = haskellPackages.pandoc-citeproc;

  eisvogel = pkgs.fetchFromGitHub {
    owner  = "Wandmalfarbe";
    repo   = "pandoc-latex-template";
    rev    = "40b00f300237780f2f5486ca88a18d987ad6bcd2";
    sha256 = "0g8ljzz4qacvjp38c3d5hraxgm93bxxsb8qkjj61b18ia4jnysxp";
  };

  csl = pkgs.fetchFromGitHub {
    owner  = "citation-style-language";
    repo   = "styles";
    rev    = "6b05f55e28e4689680bfc046247579756680c78b";
    sha256 = "0rnmsv01iz1j3rxc0z731a6kpqxnvp1dfyrd69lwgr4rzfm8acwx";
  };

  book = book;
}

