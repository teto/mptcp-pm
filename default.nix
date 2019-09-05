# {
# # TODO provide a default
#   # compiler
#   # compilerName = "ghc865";
#   nixpkgs ? import ./pinned_nixpkgs.nix
# }:
let
  # compilerName = "ghc865";

  compiler = pkgs.haskell.packages.ghc865;

  # inherit config;
  # pkgs = import layer3-nixpkgs {};
  cabal2_nixpkgs = import ./pinned_nixpkgs.nix;
  pkgs = cabal2_nixpkgs.pkgs;
in
  # pkgs.haskell.lib.doHaddock (
    compiler.callCabal2nix "mptcp-pm" ./. {}
  # )
