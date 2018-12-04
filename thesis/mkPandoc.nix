args:
let 
  commit = "5f9be606f6c8aff8a0d491d2651a59ae18be2ab9";
  sha256 = "146yg3pxg1qk8mhz1dbwpdpaxv2wh5222iclw6cmnqll24zcv9p9";
in import (builtins.fetchTarball {
  inherit sha256;
  url = "https://github.com/chisui/nix-mkPandoc/archive/${commit}.tar.gz";
}) args 
