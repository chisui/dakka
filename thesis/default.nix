with import <nixpkgs> {};

let
  eisvogel = pkgs.fetchFromGitHub {
    owner  = "Wandmalfarbe";
    repo   = "pandoc-latex-template";
    rev    = "40b00f300237780f2f5486ca88a18d987ad6bcd2";
    sha256 = "0g8ljzz4qacvjp38c3d5hraxgm93bxxsb8qkjj61b18ia4jnysxp";
  };
 
  # tests seem to be broken currently for this package
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
    texlive.combined.scheme-full
  ];

  buildPhase = ''
    pandoc thesis/main.md \
      --filter pandoc-include-code \
      --number-sections \
      --from markdown \
      --template ${eisvogel}/eisvogel.tex \
      --listings \
      --toc \
      -o result.pdf
  '';

  installPhase = ''
    mkdir $out
    mv result.pdf $out/thesis.pdf
  '';
}

