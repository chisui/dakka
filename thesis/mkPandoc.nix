args:
let 
  commit = "2700a64a1a32be2b9afbdc6485910a2421462e60";
  sha256 = "00q4spvhpj70f7g7iadk94kyb12w95wfl5v3a5m1fpjzi0i1ki3i";
in import (builtins.fetchTarball {
  inherit sha256;
  url = "https://github.com/chisui/nix-mkPandoc/archive/${commit}.tar.gz";
}) args 
