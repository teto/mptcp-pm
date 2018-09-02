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
# # { pkgs ? import (builtins.fetchTarball "https://github.com/peti/nixpkgs/archive/6f916e5209155c89c273ac08a242c058a95404b0.tar.gz" ) {} }: 
  pkgs = import <nixpkgs> { };
#   drv = pkgs.haskellPackages.callPackage ./netlink-pm-haskell.nix { };

#   # https://github.com/domenkozar/hie-nix/archive/master.tar.gz
#   hie-nix = import builtins.fetchTarball https://github.com/domenkozar/hie-nix/archive/master.tar.gz {};

#   hie = hie-nix.ihe84;
in

  # drv.env 
  pkgs.haskellPackages.ghcWithPackages(p: with p; [
    netlink 
    # hlint
    # hdevtools
  ])
  # if pkgs.lib.inNixShell then drv.env else drv
