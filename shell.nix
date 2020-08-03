# from https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md
{
  nixpkgs ? import ./pinned_nixpkgs.nix
  , compilerName ? "ghc884"
}:

let
  compiler = pkgs.haskell.packages."${compilerName}";
  pkgs = nixpkgs.pkgs;

  my_pkg = (import ./. { inherit compiler; } );
in
  pkgs.mkShell {
    name = "mptcppm";
    buildInputs = with pkgs; [ 
      ghc haskellPackages.cabal-install haskellPackages.c2hs 
    ];
  }

  # (my_pkg.envFunc { withHoogle = true; }).overrideAttrs (oa: {
  #   nativeBuildInputs = oa.nativeBuildInputs ++ (with pkgs; [
  #     haskellPackages.ghcide
  #     haskellPackages.cabal-install
  #     haskellPackages.hasktags
  #     # haskellPackages.nvim-hs-ghcid # too old, won't support nvim-hs-contrib 2
  #     # haskellPackages.gutenhasktags  # taken from my overlay
  #   ]);
  # # export HIE_HOOGLE_DATABASE=$NIX_GHC_DOCDIR as DOCDIR doesn't exist it won't work
  # # or an interesting
  # # shellHook = "eval $(grep export ${ghc}/bin/ghc)";
  # # echo "importing a custom nvim ${my_nvim}"
  # # export PATH="${my_nvim}/bin:$PATH"
  # # --package-db /home/teto/netlink-hs/dist/package.conf.inplace
  # shellHook = ''
  #   # check if it's still needed ?
  #   export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
  #   # export runghc=" "
  #   source ./run_daemon
  #   echo "to regenerate C bindings"
  #   echo "make headers_install in the linux kernel"
  #   echo "cabal clean"
  #   echo "cabal configure --extra-include-dirs=/home/teto/mptcp/build/usr/include -v3"
  #   echo "to run the daemon "
  #   echo "buildNrun"
  # '';
  # })
