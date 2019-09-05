# {
# TODO provide a default
  # compiler
  # compilerName = "ghc865";
# }:
let
  overlay = self: prev: {
      haskell = prev.haskell // {
        packageOverrides = hnew: hold: with prev.haskell.lib;{
          # hspec = hold.hspec_2_7_1;
          # hspec-core = hold.hspec-core_2_7_1;
          # hspec-discover = hold.hspec-discover_2_7_1;
          # QuickCheck = hold.QuickCheck_2_13_1;

          # from nixpkgs-stackage overlay
          # ip = pkgs.haskell.lib.dontCheck hold.ip;
          all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};

          ip = dontCheck hold.ip;
          c2hsc = dontCheck hold.c2hsc;
          wide-word = doJailbreak (hold.wide-word);

          netlink = (overrideSrc hold.netlink {
            # src = builtins.fetchGit {
            #   # url = https://github.com/ongy/netlink-hs;
            #   url = https://github.com/teto/netlink-hs;
            # };
            src = prev.fetchFromGitHub {
              owner = "teto";
              repo = "netlink-hs";
              rev = "090a48ebdbc35171529c7db1bd420d227c19b76d";
              sha256 = "sha256-qopa1ED4Bqk185b1AXZ32BG2s80SHDSkCODyoZfnft0=";
            };
          });

          # TODO change source
          # bitset = pkgs.haskell.lib.overrideSrc hold.bitset { src = pkgs.fetchFromGithub {
          #     owner = "teto";
          #     repository = "bitset";
          #     rev = "upgrade";
          #     sha256 = "0j0hrzr9b57ifwfhggpzm43zcf6wcsj8ffxv6rz7ni7ar1x96x2c";
          #   };
          # };

          # ip = pkgs.haskell.packages.stackage.lts-1321.ip;
          # QuickCheck = haskellPackagesOld.QuickCheck_2_13_1;
          # ip_1_5_0 = haskellPackagesOld.ip_1_5_0.override { };
        };
      };
    allowBroken = true;
  };

  # nixpkgs = fetchNixpkgs {
  #   owner = "layer-3-communications";
  #   repo = "nixpkgs";
  #   rev = "2ac764de78a1e63009143e2ecd88aa378002190f";
  #   sha256 = "0j0hrzr9b57ifwfhggpzm43zcf6wcsj8ffxv6rz7ni7ar1x99x2c";
  # };

  # taken from
  layer3-nixpkgs = builtins.fetchTarball {
    url = "https://github.com/layer-3-communications/nixpkgs/archive/2ac764de78a1e63009143e2ecd88aa378002190f.tar.gz";
    sha256 = "0j0hrzr9b57ifwfhggpzm43zcf6wcsj8ffxv6rz7ni7ar1x99x2c";
    # inherit sha256;
  };

  # compilerName = "ghc865";

  compiler = pkgs.haskell.packages.ghc865;

  # inherit config;
  # pkgs = import layer3-nixpkgs {};
  cabal2_nixpkgs = import ./pinned_nixpkgs.nix;
  pkgs = import cabal2_nixpkgs {  overlays = [ overlay]; };
in
  # pkgs.haskell.lib.doHaddock (
    compiler.callCabal2nix "mptcp-pm" ./. {}
  # )
