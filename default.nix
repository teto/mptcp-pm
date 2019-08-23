{ compiler }:

let
  pkgs = import <nixpkgs> {};
in
  pkgs.haskell.lib.doHaddock (compiler.callCabal2nix "mptcp-pm" ./. {})
