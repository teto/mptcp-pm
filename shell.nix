# from https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md
# can be called via
# https://github.com/Gabriel439/haskell-nix/blob/master/project4/README.md
# { 
#   # pkgs ? import <nixpkgs> {}
# compiler ? pkgs.haskell.packages.ghc864
# }:

# with pkgs;
# let
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = hnew: hold: {
          # hspec = hold.hspec_2_7_1;
          # hspec-core = hold.hspec-core_2_7_1;
          # hspec-discover = hold.hspec-discover_2_7_1;
          # QuickCheck = hold.QuickCheck_2_13_1;

          # from nixpkgs-stackage overlay
          # ip = pkgs.haskell.lib.dontCheck hold.ip;

          c2hsc = pkgs.haskell.lib.dontCheck hold.c2hsc;
          wide-word = pkgs.haskell.lib.doJailbreak (hold.wide-word);

          # ip = pkgs.haskell.packages.stackage.lts-1321.ip;
          # QuickCheck = haskellPackagesOld.QuickCheck_2_13_1;
          # ip_1_5_0 = haskellPackagesOld.ip_1_5_0.override { };
        };
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
  compilerName = "ghc865";

  compiler = pkgs.haskell.packages."${compilerName}";

  # inherit config;
  # pkgs = import <nixpkgs> {  inherit config; };
  # pkgs = import layer3-nixpkgs {};
  pkgs = localPkgs;

  # localPkgs = import <nixpkgs> {  inherit config; };
  localPkgs = import <nixpkgs> {};

  # my_nvim = localPkgs.genNeovim  [ ] { withHaskell = true; };

in
  with pkgs;

  compiler.shellFor {
  # the dependencies of packages listed in `packages`, not the
  packages = p: with p; [
    (import ./. { inherit compiler;})
  ]
  ++ [

  ]
  ;
  withHoogle = true;
  nativeBuildInputs = [
    all-hies.versions."${compilerName}"

    haskellPackages.cabal-install
    # haskellPackages.bytestring-conversion
    # haskellPackages.gutenhasktags  # taken from my overlay
    # haskellPackages.haskdogs # seems to build on hasktags/ recursively import things
    haskellPackages.hasktags
    haskellPackages.nvim-hs-ghcid # too old, won't support nvim-hs-contrib 2

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

    echo "to regenerate C bindings"
    echo "make headers_install in the linux kernel"
    echo "cabal clean"
    echo "cabal configure --package-db /home/teto/netlink-hs/dist/package.conf.inplace --extra-include-dirs=/home/teto/mptcp/build/usr/include -v3"

    echo "to run the daemon "
    echo "buildNrun"
  '';

  }
