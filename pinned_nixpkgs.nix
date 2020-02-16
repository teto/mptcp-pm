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
          # wide-word = doJailbreak (hold.wide-word);

         quickcheck-classes = hold.quickcheck-classes_0_6_4_0;

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

  # pinned nixpkgs before cabal 3 becomes the default else hie fails
  nixpkgs = import <nixpkgs> 
  # nixpkgs = import (builtins.fetchTarball {
  #     name = "unstable-before-cabal-3";
  #     url = "https://github.com/nixos/nixpkgs/archive/e1eedf29e5d22e6824e614d75449b75a2e3455d6.tar.gz";
  #     sha256 = "1v237cgfkd8sb5f1r08sms1rxygjav8a1i1jjjxyqgiszzpiwdx7";
  # }) 
  {  overlays = [ overlay]; config = {allowBroken = true;}; };
in
  nixpkgs
