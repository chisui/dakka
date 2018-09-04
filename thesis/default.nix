{ pkgs     ? import ../nixpkgs.pinned.nix 
, mkPandoc ? import ./mkPandoc.nix { inherit pkgs; }
, eisvogel ? import ./nix-mkPandoc/eisvogel.latex.nix { inherit pkgs; }
, jcis-csl ? import ./nix-mkPandoc/journal-of-computer-information-systems.csl.nix { inherit pkgs; }
, pandoc-citeproc ? pkgs.haskellPackages.pandoc-citeproc
, date ? "2018-??-??"
, sha256 ? null
, commit ? null
, commitHash ? if commit != null then (builtins.fromJSON commit).object.sha else null
, verbose ? false
}:
with pkgs.lib;
mkPandoc.mkPandoc {
  name         = "dakka-thesis.pdf";
  version      = "0.2.0";
  src          = ./.;
  documentFile = ./main.md;
  bibliography = ./bibliography.bib;
  filters      = [ pandoc-citeproc ];
  toc          = true;
  template = mkPandoc.templates.eisvogel;
  csl      = mkPandoc.csls.journal-of-computer-information-systems;
  listings = true;
  top-level-division = "section";
  number-sections = true;
  variables = {
    inherit date;
    geometry     = "left=3.5cm, right=2.5cm";
    toc-own-page = true;
    titlepage    = true;
  };
  include-before-body = if commitHash == null || sha256 == null then null else (builtins.toFile "dakka-thesis-hashes.tex" ''
    \begin{footnotesize}
    \begin{tabular}{ r l }
      {\bf Commit:} & \verb|${commitHash}| \\
      {\bf sha256:} & \verb|${sha256}| \\
    \end{tabular}
    \\
    To browse the repository at this commit visit \url{https://github.com/chisui/dakka/tree/${commitHash}}.
    \\
    To verify the hash run:
    \end{footnotesize}
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
