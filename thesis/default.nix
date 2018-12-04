{ pkgs     ? import ../nixpkgs.pinned.nix 
, mkPandoc ? import ./mkPandoc.nix { inherit pkgs; }
, pandoc-citeproc ? pkgs.haskellPackages.pandoc-citeproc
, date ? "2018-??-??"
, sha256 ? null
, commit ? null
, commitHash ? if commit != null then (builtins.fromJSON commit).object.sha else null
, verbose ? false
, book ? false
}:
mkPandoc {
  name         = "dakka-thesis.pdf";
  version      = "0.2.0";
  src          = ./.;
  documentFile = ./main.md;
  bibliography = ./bibliography.bib;
  filters      = [ pandoc-citeproc ];
  template     = mkPandoc.template.latex.eisvogel;
  csl          = mkPandoc.csls.journal-of-computer-information-systems;

  listings     = true;
  toc          = true;
  top-level-division = if book then "chapter" else "section";
  number-sections = true;
  variables = {
    inherit date book;
    geometry     = if book then "left=3.5cm, right=2.5cm" else "";
    toc-own-page = true;
    titlepage    = true;
    listings-no-page-break = true;
  };
  include-after-body = if commitHash == null || sha256 == null then null
  else (builtins.toFile "dakka-thesis-hashes.tex" ''
    \${if book then "chapter" else "section"}{Appendix}

    All code produced including this thesis itself can be found on github at
    \url{https://github.com/chisui/dakka}. This document is based on the
    following commit.
    
    \begin{footnotesize}
    \begin{tabular}{ r l }
      {\bf Commit:} & \verb|${commitHash}| \\
      {\bf sha256:} & \verb|${sha256}| \\
    \end{tabular}
    \end{footnotesize}

    To browse the repository at this commit visit

    \url{https://github.com/chisui/dakka/tree/${commitHash}}.

    To verify the hash download the tarball, unpack it and hash its contents by running:

    \begin{scriptsize}
    \begin{verbatim}
    nix-prefetch-url --unpack \
      https://github.com/chisui/dakka/archive/${commitHash}.tar.gz \
      ${sha256}
    \end{verbatim}
    \end{scriptsize}
  '');
  inherit verbose; 
}
