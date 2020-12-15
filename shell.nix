# from https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md
{
  nixpkgs ? import ./pinned_nixpkgs.nix
  , compilerName ? "ghc884"
}:

let
  compiler = pkgs.haskell.packages."${compilerName}";
  pkgs = nixpkgs.pkgs;

  # my_pkg = (import ./. { inherit compiler; } );
in
  pkgs.mkShell {
    name = "mptcppm";
    buildInputs = with pkgs; [
      ghc
      haskellPackages.cabal-install
      haskellPackages.c2hs
      haskellPackages.stylish-haskell
      haskellPackages.hlint
      haskellPackages.haskell-language-server
      haskellPackages.hasktags
    ];

    # ASAN_OPTIONS=abort_on_error=1
    # halt_on_error=0"
    # echo " let g:airline_section_y = airline#section#create_right(['lsp_status'])" > .nvimrc
    # echo "set statusline=%!StatusLSP()" > .nvimrc
    shellHook = ''
      # check if it's still needed ?
      export NVIM_LOG_FILE=/tmp/log

      export ASAN_OPTIONS="log_path=./test.log:abort_on_error=1"
      export UBSAN_OPTIONS=print_stacktrace=1
      export VIMRUNTIME=/home/teto/neovim3/runtime

    '';
  }

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
