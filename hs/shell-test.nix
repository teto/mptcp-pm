# from https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md
# can be called via
# nix-shell --argstr compiler ghc784
{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:
# with import <nixpkgs> {};

with nixpkgs;
let

  # Override netlink to fetch the fix from the repository
  # haskell.packages."${compiler}" = haskell.packages."${compiler}".override {
  #   overrides = self: super: {
  #     "netlink" = prev.haskell.lib.overrideSrc hprev.netlink {
  #       src = prev.fetchFromGitHub {
  #         owner = "ongy";
  #         repo = "netlink-hs";
  #         rev = "8e7a285f7e4cee0a7f908e431559c87c2f228783";

  #         sha256 = "05hq41zh5msm06gfgfjvf1lq1qnqg1l2ng1ywiikkck8msc3mmx1";
  #       };
  #     };
  #   };
  # };

  hie_remote = builtins.fetchTarball {
    url    = https://github.com/domenkozar/hie-nix/tarball/master;
    # "https://github.com/NixOS/nixpkgs/archive/3389f23412877913b9d22a58dfb241684653d7e9.tar.gz";
    # sha256 = "0wgm7sk9fca38a50hrsqwz6q79z35gqgb9nw80xz7pfdr4jy9pf7";
  };
  #  haskellPackages.hie

  # todo make it automatic depending on nixpkgs' ghc
# pkgs.haskell.packages.ghc864

  # TODO how to retrieve the compiler there
  hie = (import hie_remote {
    # compiler = pkgs.haskell.compiler.ghc864;
  } ).hie86;

  # hie = (import "${hie_remote}/ghc-8.6.nix" {
  #   compiler = pkgs.haskell.compiler.ghc864;
  # } ).haskell-ide-engine;


  # TODO override
  # "netlink" = prev.haskell.lib.overrideSrc hprev.netlink {
  #    src = prev.fetchFromGitHub {
  #     owner = "ongy";
  #     repo = "netlink-hs";
  #     rev = "8e7a285f7e4cee0a7f908e431559c87c2f228783";

  #     sha256 = "05hq41zh5msm06gfgfjvf1lq1qnqg1l2ng1ywiikkck8msc3mmx1";
  #   };
  # };

in

  # haskellPackages.shellFor {
  haskell.packages."${compiler}".shellFor {
  # the dependencies of packages listed in `packages`, not the
  packages = p: with p; [
    (import ./. { inherit compiler;})
  ];
  withHoogle = true;
  # haskellPackages.stack
  nativeBuildInputs = [
    hie
    haskellPackages.cabal-install
    # haskellPackages.bytestring-conversion
    haskellPackages.gutenhasktags
    haskellPackages.haskdogs # seems to build on hasktags/ recursively import things
    haskellPackages.hasktags
    haskellPackages.nvim-hs
    haskellPackages.nvim-hs-ghcid

    # for https://hackage.haskell.org/package/bytestring-conversion-0.2/candidate/docs/Data-ByteString-Conversion-From.html
  ];

  # export HIE_HOOGLE_DATABASE=$NIX_GHC_DOCDIR as DOCDIR doesn't exist it won't work
  shellHook = ''
    # check if it's still needed ?
    export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
    # export runghc=" "
    source ./run_daemon
  '';
}
