with import <nixpkgs> {};

let
  hie_remote = builtins.fetchTarball {
    url    = https://github.com/domenkozar/hie-nix/tarball/master;
    # "https://github.com/NixOS/nixpkgs/archive/3389f23412877913b9d22a58dfb241684653d7e9.tar.gz";
    # sha256 = "0wgm7sk9fca38a50hrsqwz6q79z35gqgb9nw80xz7pfdr4jy9pf7";
  };
  #  haskellPackages.hie
  hie = (import hie_remote {} ).hie84;

in
haskellPackages.shellFor {
  # the dependencies of packages listed in `packages`, not the
  packages = p: with p; [
    netlink-pm 
  ];
  withHoogle = true;
  # haskellPackages.stack 
  nativeBuildInputs = [ 
    hie 
    haskellPackages.cabal-install 
    haskellPackages.bytestring-conversion

    # for https://hackage.haskell.org/package/bytestring-conversion-0.2/candidate/docs/Data-ByteString-Conversion-From.html
  ];

  # export HIE_HOOGLE_DATABASE=$NIX_GHC_DOCDIR as DOCDIR doesn't exist it won't work
  shellHook = ''
    export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
  '';
}
