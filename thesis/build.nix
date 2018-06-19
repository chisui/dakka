{ book ? false }:

with import <nixpkgs> {};

(import ./thesis.nix) {
  mkDerivation = stdenv.mkDerivation;

  texlive = texlive.combined.scheme-full;

  pandoc = pandoc;
  pandoc-citeproc = haskellPackages.pandoc-citeproc;
  # doesn't currently build until https://github.com/owickstrom/pandoc-include-code/pull/12 is merged 
  pandoc-include-code = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.pandoc-include-code;

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

