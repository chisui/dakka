{ pkgs     ? import ../nixpkgs.pinned.nix 
, mkPandoc ? import ./nix-mkPandoc/mkPandoc.nix { inherit pkgs; }
, eisvogel ? import ./nix-mkPandoc/eisvogel.latex.nix { inherit pkgs; }
, jcis-csl ? import ./nix-mkPandoc/journal-of-computer-information-systems.csl.nix { inherit pkgs; }
, pandoc-citeproc ? pkgs.haskellPackages.pandoc-citeproc
, date ? "2018-??-??"
, verbose ? false
}:
mkPandoc {
  name         = "dakka-thesis.pdf";
  version      = "0.2.0";
  src          = ./.;
  documentFile = ./main.md;
  bibliography = ./bibliography.bib;
  filters      = [ pandoc-citeproc ];
  toc = true;
  listings = true;
  top-level-division = "section";
  number-sections = true;
  template = eisvogel;
  csl = jcis-csl;
  metadatas = { inherit date; };
  inherit verbose;
}
