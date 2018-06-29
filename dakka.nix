{ mkDerivation, base, bytestring, mtl, stdenv, transformers, containers, contravariant, distributed-process, distributed-static, network-transport-tcp, should-not-typecheck, tasty, tasty-quickcheck, tasty-hunit }:
  
mkDerivation {
  pname = "dakka";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base bytestring transformers mtl contravariant containers distributed-process distributed-static network-transport-tcp should-not-typecheck tasty tasty-quickcheck tasty-hunit ];
  description = "dakka";                                                                                                                   
  license = stdenv.lib.licenses.mit;                                                                                                       
}

