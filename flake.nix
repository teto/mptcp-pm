{
  description = "A multipath TCP subflow management software.";
  nixConfig = {
    substituters = [  https://hydra.iohk.io ];
    bash-prompt = "toto";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    hls.url = "github:haskell/haskell-language-server?rev=1ba88ab9eca1da29cd4fee7d2084eba4074fbe47";
    # haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, hls }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let

      compilerVersion = "8104";

      pkgs = import nixpkgs {
        inherit system;
        inherit overlays;
        # config = { allowUnfree = true; allowBroken = true; };
      };

      overlays = [
        # haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          # mptcp-pm =
          #   final.haskell-nix.project' {
          #     src = ./.;
          #     compiler-nix-name = "ghc${compilerVersion}";
          #   };
        })
      ];
      flake = pkgs.mptcp-pm.flake {};
      haskellOverlay = hnew: hold: with pkgs.haskell.lib; {
          ip = unmarkBroken (dontCheck hold.ip);
          bytebuild = unmarkBroken (dontCheck hold.bytebuild);
          wide-word = unmarkBroken (dontCheck hold.wide-word);
          co-log-polysemy = unmarkBroken (doJailbreak (hold.co-log-polysemy));
          polysemy = unmarkBroken (doJailbreak (hold.polysemy));
          contiguous = unmarkBroken (doJailbreak (hold.contiguous));
          # cryptohash-md5 = doJailbreak (hold.cryptohash-md5);
          # cryptohash-sha1 = doJailbreak (hold.cryptohash-sha1);
          netlink = (overrideSrc hold.netlink {
            src = pkgs.fetchFromGitHub {
              owner = "teto";
              repo = "netlink-hs";
              rev = "090a48ebdbc35171529c7db1bd420d227c19b76d";
              sha256 = "sha256-qopa1ED4Bqk185b1AXZ32BG2s80SHDSkCODyoZfnft0=";
            };
          });
        };

      in rec {
    # haskell.compiler."ghc"
    # packages.mptcp-pm = flake.packages."mptcp-pm:exe:mptcp-manager";

    packages.mptcp-pm = pkgs.haskellPackages.callCabal2nix "mptcp-pm" ./. {
      # returnShellEnv = false;
      # withHoogle = true;
      overrides = haskellOverlay;
      modifier = drv:
        pkgs.haskell.lib.addBuildTools drv (with pkgs;
        [
          # ghcid
          haskellPackages.cabal-install
          haskellPackages.c2hs
          haskellPackages.stylish-haskell
          haskellPackages.hlint
          # haskellPackages.haskell-language-server
          haskellPackages.hasktags
          hls.packages."${system}"."haskell-language-server-${compilerVersion}"
        ]);
    };

    defaultPackage = packages.mptcp-pm;

    # This is used by `nix develop .` to open a shell for use with
    # `cabal`, `hlint` and `haskell-language-server`
    # devShell = pkgs.mptcp-pm.shellFor {
    #   tools = {
    #     cabal = "latest";
    #     hlint = "latest";
    #     haskell-language-server = "latest";
    #   };
    # };

    devShell = pkgs.haskellPackages.developPackage {
      root = ./.;
      name = "mptcp-pm";
      returnShellEnv = false;
      withHoogle = true;
      overrides = haskellOverlay;
      modifier = drv:
        pkgs.haskell.lib.addBuildTools drv (with pkgs;
        [
          # ghcid
          haskellPackages.cabal-install
          haskellPackages.c2hs
          haskellPackages.stylish-haskell
          # haskellPackages.hlint # in hls ?
          haskellPackages.hasktags
          hls.packages."${system}"."haskell-language-server-${compilerVersion}"
        ]);
    };
  });
}
