let
  overlay = self: prev: {
      haskell = prev.haskell // {
        packageOverrides = hnew: hold: with prev.haskell.lib;{

          ip = dontCheck hold.ip;
          bytebuild = dontCheck hold.bytebuild;

          # ip = dontCheck overrideSrc hold.ip {
          #   src = prev.fetchFromGitHub {
          #     owner = "andrewthad";
          #     repo = "haskell-ip";
          #     rev = "1.7.2";
          #     # sha256 = 
          #     sha256 = "sha256-OL5KkVsoGAZskfwj8HAq2aBh60F2TW+WnAUfTjiekNc=";
          #     # sha256 = "sha256-QGpMskYOcUR4o/IC30etrTEAkdvevpOOi4chWbEw5Ik=";
          #   };
          # };

          # c2hsc = appendPatch hold.c2hsc (prev.fetchpatch {
          #   url = "https://github.com/jwiegley/c2hsc/commit/490ecab202e0de7fc995eedf744ad3cb408b53cc.patch";
          #   # sha256 = "12g462qmj8c7if797gnyvf8h0cddmm3xy0pjldw48w8f8sr4qsj0";
          #   # sha256 = "sha256-QGpMskYOcUR4o/IC30etrTEAkdvevpOOi4chWbEw5Ik=";
          #   sha256 = "sha256-B0OcZkVrLTBCCBJL+b4vfxSTBDfTT5lKYOie3Pe187A=";
          # });

          # can be released on more recent nixplks
          # wide-word = doJailbreak (hold.wide-word);
          # quickcheck-classes = hold.quickcheck-classes_0_6_4_0;

          # for newer nixpkgs (March 2020)
          # base-compat = doJailbreak (hold.base-compat);
          # time-compat = doJailbreak (hold.time-compat);

          # rebase needs time == 1.9.*
          # time is a core package of ghc
          # time = hold.time_1_10;
          # time = null;
          # rebase = doJailbreak (hold.rebase);

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

        };
      };
  };

  # pinned nixpkgs before cabal 3 becomes the default else hie fails
  nixpkgs = import <nixpkgs>
  # nixpkgs = import (builtins.fetchTarball {
  #     name = "before-libc-update";
  #     url = "https://github.com/nixos/nixpkgs/archive/fa7445532900f2555435076c1e7dce0684daa01a.tar.gz";
  #     sha256 = "1hbf7kmbxmd19hj3kz9lglnyi4g20jjychmlhcz4bx1limfv3c3r";
  # })
  {  overlays = [ overlay]; config = {allowBroken = true;}; };
in
  nixpkgs
