{
  description = "A multipath TCP subflow management software.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    hls.url = "github:haskell/haskell-language-server";
  };

  outputs = { self, nixpkgs, flake-utils, hls }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let

      compilerVersion = "8104";

      pkgs = import nixpkgs {
      # .legacyPackages."${system}";
        inherit system;
        config = { allowUnfree = true; allowBroken = true; };
      };

      haskellOverlay = hnew: hold: with pkgs.haskell.lib; {

          ip = unmarkBroken (dontCheck hold.ip);
          bytebuild = unmarkBroken (dontCheck hold.bytebuild);
          wide-word = unmarkBroken (dontCheck hold.wide-word);

          co-log-polysemy = doJailbreak (hold.co-log-polysemy);

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
    packages.mptcppm = pkgs.haskellPackages.developPackage {
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
          haskellPackages.hlint
          # haskellPackages.haskell-language-server
          haskellPackages.hasktags
          hls.packages."${system}"."haskell-language-server-${compilerVersion}"
        ]);
    };

    defaultPackage = packages.mptcppm;

    # devShell = pkgs.haskellPackages.developPackage {
    #   root = ./.;
    #   name = "mptcp-pm";
    #   returnShellEnv = false;
    #   withHoogle = true;
    #   overrides = haskellOverlay;
    #   modifier = drv:
    #     pkgs.haskell.lib.addBuildTools drv (with pkgs;
    #     [
    #       # ghcid
    #       haskellPackages.cabal-install
    #       haskellPackages.c2hs
    #       haskellPackages.stylish-haskell
    #       haskellPackages.hlint
    #       # haskellPackages.haskell-language-server
    #       haskellPackages.hasktags
    #       hls.packages."${system}"."haskell-language-server-${compilerVersion}"
    #     ]);
    # };
  });
}
