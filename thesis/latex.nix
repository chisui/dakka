{ pkgs     ? import ../nixpkgs.pinned.nix 
, mkPandoc ? import ./mkPandoc.nix { inherit pkgs; }
, pandoc-citeproc ? pkgs.haskellPackages.pandoc-citeproc
, verbose ? false
}:
mkPandoc {
  name         = "dakka-thesis.pdf";
  version      = "0.2.0";
  src          = ./.;
  documentFile = ./main.latex;
  bibliography = ./bibliography.bib;
  filters      = [ pandoc-citeproc ];
  template     = mkPandoc.template.latex.eisvogel;
  csl          = mkPandoc.csls.journal-of-computer-information-systems;
  inherit verbose; 
}
