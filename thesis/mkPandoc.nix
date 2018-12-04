args:
let 
  commit = "fed6ecb3fe8fc0d953038a62b829abc908627f16";
  sha256 = "1hh2f06n6lqmzpvv22vinkmj70xqy87rx1fz2swvdbznb5ylknx4";
in import (builtins.fetchTarball {
  inherit sha256;
  url = "https://github.com/chisui/nix-mkPandoc/archive/${commit}.tar.gz";
}) args 
