with import <nixpkgs> {};

let
  book = true;

  eisvogel = pkgs.fetchFromGitHub {
    owner  = "Wandmalfarbe";
    repo   = "pandoc-latex-template";
    rev    = "40b00f300237780f2f5486ca88a18d987ad6bcd2";
    sha256 = "0g8ljzz4qacvjp38c3d5hraxgm93bxxsb8qkjj61b18ia4jnysxp";
  };

  csl-repo = pkgs.fetchFromGitHub {
    owner  = "citation-style-language";
    repo   = "styles";
    rev    = "6b05f55e28e4689680bfc046247579756680c78b";
    sha256 = "0rnmsv01iz1j3rxc0z731a6kpqxnvp1dfyrd69lwgr4rzfm8acwx";
  };
 
  # doesn't currently build until https://github.com/owickstrom/pandoc-include-code/pull/12 is merged 
  pandoc-include-code = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.pandoc-include-code;
in

stdenv.mkDerivation {
  name    = "dakka-thesis";
  version = "0.0.1";

  # to make includes work
  src = ./..;

  buildInputs = [
    pandoc
    pandoc-include-code
    haskellPackages.pandoc-citeproc
    texlive.combined.scheme-full
  ];

  buildPhase = ''
    ${if book
      then "sed -e 's/scrartcl/scrbook/g' ${eisvogel}/eisvogel.tex > template.tex"
      else ""
    }

    pandoc thesis/main.md \
      --from markdown \
      --listings \
      --toc \
      --filter pandoc-include-code \
      --filter pandoc-citeproc \
      --bibliography ./thesis/bibliography.bib \
      --csl ${csl-repo}/journal-of-computer-information-systems.csl \
      --template ${if book then "./template.tex" else "${eisvogel}/eisvogel.tex" } \
      --number-sections \
      --top-level-division=${if book then "chapter" else "section"} \
      -o result.pdf
  '';

  installPhase = ''
    mkdir $out
    mv result.pdf $out/thesis.pdf
  '';
}

