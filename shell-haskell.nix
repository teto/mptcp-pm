# https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure
# { nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
# let
#   inherit (nixpkgs) pkgs;

#  # haskellPackages.ghcWithHoogle (pkgs: [pkgs.netlink])
#   ghc = pkgs.haskell.packages.${compiler}.ghcWithHoogle (ps: with ps; [
#           netlink
#         ]);
# in
# pkgs.stdenv.mkDerivation {
#   name = "my-haskell-env-0";
#   buildInputs = [ ghc ];
#   shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
# }
let
  pkgs = import <nixpkgs> { };
in
  { project1 = pkgs.haskellPackages.callPackage ./netlink-pm-haskell.nix { };
  }
