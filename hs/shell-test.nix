with import <nixpkgs> {};

haskellPackages.shellFor {
  packages = p: with p; [ haskellPackages.netlink-pm ];
  withHoogle = true;
  nativeBuildInputs = [ cabal-install ];
}
