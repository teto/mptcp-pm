# from https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md
# can be called via
# nix-shell --argstr compiler ghc784
{ pkgs ? import <nixpkgs> {}
, compiler ?
# pkgs.haskell.packages.ghc863Binary
pkgs.haskell.packages.ghc864
}:

with pkgs;
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

  my_nvim = genNeovim  [ ] { withHaskell = true; };

  # hie-nixpkgs= import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/70cff41febf8d27cb55eb11f5e7d54af2a3357db.tar.gz ) {};
in

  # haskellPackages.shellFor {
  compiler.shellFor {
  # the dependencies of packages listed in `packages`, not the
  packages = p: with p; [
    (import ./. { inherit compiler;})
  ];
  withHoogle = true;
  # haskellPackages.stack
  nativeBuildInputs = [
    # # defined from my overlay, builds instead of using cachix
    # hie-nixpkgs.haskellPackages.hie
    # hie
    # haskellPackages.all-hies.versions.ghc864
    all-hies.versions.ghc864

    haskellPackages.cabal-install
    # haskellPackages.bytestring-conversion
    haskellPackages.gutenhasktags
    haskellPackages.haskdogs # seems to build on hasktags/ recursively import things
    haskellPackages.hasktags
    haskellPackages.nvim-hs
    # haskellPackages.nvim-hs-ghcid

    # for https://hackage.haskell.org/package/bytestring-conversion-0.2/candidate/docs/Data-ByteString-Conversion-From.html
  ];

  # export HIE_HOOGLE_DATABASE=$NIX_GHC_DOCDIR as DOCDIR doesn't exist it won't work
  # or an interesting
  # shellHook = "eval $(grep export ${ghc}/bin/ghc)";
  # echo "importing a custom nvim ${my_nvim}"
  # export PATH="${my_nvim}/bin:$PATH"
  shellHook = ''

    # check if it's still needed ?
    export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
    # export runghc=" "
    source ./run_daemon
    export PATH="${my_nvim}/bin:$PATH"

    echo "to regenerate C bindings"
    echo "make headers_install in the linux kernel"
    echo "cabal clean"
    echo "cabal configure --package-db /home/teto/netlink-hs/dist/package.conf.inplace --extra-include-dirs=/home/teto/mptcp2/build/usr/include -v3"

    echo "to run the daemon "
    echo "buildNrun"
  '';

  }
