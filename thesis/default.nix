{ nixpkgsVersion ? builtins.fromJSON (builtins.readFile ../nixpkgs-version.json)
, nixpkgs ? builtins.fetchTarball {
    inherit (nixpkgsVersion) sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
  }
, pkgs ? import nixpkgs {}
, mkPandoc ? import ./mkPandoc.nix { inherit pkgs; }
, template ? import ./eisvogel.latex.nix { inherit pkgs; }
, csl ? import ./journal-of-computer-information-systems.csl.nix { inherit pkgs; }
, pandoc-citeproc ? pkgs.haskellPackages.pandoc-citeproc
}:
mkPandoc {
  name         = "dakka-thesis.pdf";
  version      = "0.2.0";
  src          = ./main.md;
  bibliography = ./bibliography.bib;
  filters      = [ pandoc-citeproc ];
  toc = true;
  listings = true;
  top-level-division = "section";
  number-sections = true;
  inherit csl template;
}
