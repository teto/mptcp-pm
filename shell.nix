# from https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md
# can be called via
# https://github.com/Gabriel439/haskell-nix/blob/master/project4/README.md
{
  nixpkgs ? import (builtins.fetchTarball {
    name = "unstable-before-cabal-3";
    url = "https://github.com/nixos/nixpkgs/archive/b8f9e09ad17eac2fb4c13105638a86d98281f546.tar.gz";
    sha256 = "16pd2fr0l446yjfyqkp492fpkd810lv0lddfziwpw2av31ha7srf";
})

  # ./pinned_nixpkgs.nix
  # pkgs ? import <nixpkgs> {}
  , compilerName ? "ghc865"
}:
  # localPkgs = import <nixpkgs> {  overlays = [ overlay]; };
  # my_nvim = localPkgs.genNeovim  [ ] { withHaskell = true; };
# in

  let
    compiler = pkgs.haskell.packages."${compilerName}";
    pkgs = (nixpkgs {}).pkgs;
  in

  compiler.shellFor {
  # the dependencies of packages listed in `packages`, not the
  packages = p: with p; [
    # (import ./. { inherit compiler;})
    (import ./. )
  ]
  ++ [

  ]
  ;
  withHoogle = true;
  nativeBuildInputs = with pkgs; [
    haskellPackages.all-hies.versions."${compilerName}"

    haskellPackages.cabal-install
    haskellPackages.hasktags
    haskellPackages.nvim-hs-ghcid # too old, won't support nvim-hs-contrib 2

    # haskellPackages.gutenhasktags  # taken from my overlay
    # haskellPackages.haskdogs # seems to build on hasktags/ recursively import things
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

    echo "to regenerate C bindings"
    echo "make headers_install in the linux kernel"
    echo "cabal clean"
    echo "cabal configure --package-db /home/teto/netlink-hs/dist/package.conf.inplace --extra-include-dirs=/home/teto/mptcp/build/usr/include -v3"

    echo "to run the daemon "
    echo "buildNrun"
  '';

  }
