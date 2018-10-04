args:
let 
  commit = "ffd516767668114600c19adb31e64e337262b135";
  sha256 = "03dai40bmvzz51fyp56h9qk6y8fgc468m4w4ril9q467ng7pqji4";
in import (builtins.fetchTarball {
  inherit sha256;
  url = "https://github.com/chisui/nix-mkPandoc/archive/${commit}.tar.gz";
}) args 
