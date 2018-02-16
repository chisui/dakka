{ mkDerivation, base, containers, singletons, stdenv, transformers
}:
mkDerivation {
  pname = "dakka";
  version = "0.3.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers singletons transformers
  ];
  description = "Minimal akka-inspired, dependently typed actor library";
  license = stdenv.lib.licenses.mit;
}
