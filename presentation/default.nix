{ pkgs     ? import ../nixpkgs.pinned.nix 
, mkPandoc ? import ../thesis/mkPandoc.nix { inherit pkgs; }
, verbose ? true 
}:
mkPandoc {
  name       = "dakka-presentation.html";
  version    = "0.1.0";
  src        = ./index.md;
  to         = "revealjs";
  standalone = true;
  incremental = true;
  variables = {
    revealjs-url = "https://revealjs.com";
  };
  inherit verbose; 
}
