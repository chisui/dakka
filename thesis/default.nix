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
  toc          = true;
  template = eisvogel;
  csl      = jcis-csl;
  listings = true;
  top-level-division = "section";
  number-sections = true;
  variables = {
    inherit date;
    geometry     = "left=3.5cm, right=2.5cm";
    toc-own-page = true;
    titlepage    = true;
  };
  inherit verbose;
}
