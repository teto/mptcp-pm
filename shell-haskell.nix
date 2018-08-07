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

# cool link http://www.kuznero.com/posts/nixos/haskell-devexp-in-nixos.html
let
  pkgs = import <nixpkgs> { };
  drv = pkgs.haskellPackages.callPackage ./netlink-pm-haskell.nix { };

in

  if pkgs.lib.inNixShell then drv.env else drv
