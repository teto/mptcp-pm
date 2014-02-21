let

  compiler = pkgs.haskell.packages.ghc865;

  cabal2_nixpkgs = import ./pinned_nixpkgs.nix;
  pkgs = cabal2_nixpkgs.pkgs;
in
  compiler.callCabal2nix "mptcp-pm" ./. {}
