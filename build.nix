{ mkDerivation, base, containers, singletons, stdenv, transformers
}:
mkDerivation {
  pname = "hakka";
  version = "0.3.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers singletons transformers
  ];
  description = "Minimal akka-inspired actor library";
  license = stdenv.lib.licenses.mit;
}
