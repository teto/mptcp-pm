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
        overrides = haskellPackagesNew: hold: rec {
          hspec = hold.hspec_2_7_1;
          hspec-core = hold.hspec-core_2_7_1;
          hspec-discover = hold.hspec-discover_2_7_1;
          QuickCheck = hold.QuickCheck_2_13_1;
          # QuickCheck = haskellPackagesOld.QuickCheck_2_13_1;
          # ip_1_5_0 = haskellPackagesOld.ip_1_5_0.override { };
        };
      };
    };
    allowBroken = true;
  };

  compiler = pkgs.haskell.packages.ghc864;

  # inherit config;
  pkgs = import <nixpkgs> {  inherit config; };

  my_nvim = pkgs.genNeovim  [ ] { withHaskell = true; };

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
    all-hies.versions.ghc864
    haskellPackages.ip_1_5_0 

    haskellPackages.cabal-install
    # haskellPackages.bytestring-conversion
    # haskellPackages.gutenhasktags
    # haskellPackages.haskdogs # seems to build on hasktags/ recursively import things
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
