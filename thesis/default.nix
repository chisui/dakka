let
  thesis = book: 

    with import <nixpkgs> {};

    (import ./thesis.nix) {
      inherit book pandoc;
      inherit (stdenv) mkDerivation;
      inherit (haskellPackages) pandoc-citeproc;

      texlive = texlive.combine {
        inherit (texlive)
          scheme-basic
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

      eisvogel = pkgs.fetchurl {
        url    = "https://raw.githubusercontent.com/Wandmalfarbe/pandoc-latex-template/540173c741da6cc466fb698bc49d78d4f196c1a9/eisvogel.tex";
        sha256 = "c81d55720e62d40963d33ea41f10def78059fb5f9d7359a33a2cf3db2411a6dd";
      };

      csl = pkgs.fetchurl {
        url    = "https://raw.githubusercontent.com/citation-style-language/styles/73a405779d590a45424650c712b43e6417b412c0/journal-of-computer-information-systems.csl";
        sha256 = "803069db5fedba92f478ba8b91a3235cb0b892e1d8edc57b611b832d3f763b95";
      };
    };
in
  thesis false // { book = thesis true; }

