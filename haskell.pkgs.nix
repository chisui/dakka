{ pkgs ? import ./nixpkgs.pinned.nix }:
with pkgs.haskell.lib;
pkgs.haskell.packages.ghc865.override {
  overrides = hsSelf: hsSuper: {

    network = dontCheck hsSelf.network_2_6_3_1;
    network-transport-tcp = doJailbreak hsSuper.network-transport-tcp;

    # broken tests
    # until https://github.com/haskell-distributed/rank1dynamic/pull/23 is merged
    rank1dynamic = dontCheck hsSuper.rank1dynamic;

    # too restrictive containers version bounds
    # until https://github.com/haskell-distributed/distributed-static/issues/22 is resolved
    distributed-static = doJailbreak hsSuper.distributed-static;

    # hackage version is broken 
    distributed-process = hsSelf.callCabal2nix "distributed-process" (
      builtins.fetchTarball {
        url    = "https://github.com/haskell-distributed/distributed-process/archive/660d554f6acd2dba8b605c84e8fa69e45708bc14.tar.gz";
        sha256 = "0c71b3nc19zic9xiirkc41znv93f9j9qlf2kn89mjjyh9w7dazsn";
      }
    ) {};

    # newer polysemy version
    polysemy = hsSelf.callCabal2nix "polysemy" (
      builtins.fetchTarball {
        url    = "https://github.com/isovector/polysemy/archive/b125a9838f0bf48db3067507ef87cb5729a628f1.tar.gz";
        sha256 = "1l3phasvbfdp9fbf47m4dfmd003pc7bl61mppxml6m07cf6vy6w7";
      }
    ) {};
  };
}
