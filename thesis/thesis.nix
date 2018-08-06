{ mkDerivation, pandoc, pandoc-citeproc, texlive, eisvogel, csl, book }:

mkDerivation {
  name    = "dakka-thesis";
  version = "0.0.1";

  # to make includes work
  src = ./..;

  buildInputs = [
    pandoc
    pandoc-citeproc
    texlive
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
      --filter pandoc-citeproc \
      --bibliography ./thesis/bibliography.bib \
      --csl ${csl}/journal-of-computer-information-systems.csl \
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

