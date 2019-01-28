with import <nixpkgs> {};

haskellPackages.callCabal2nix "netlink-pm" ./. {}

# let
#   drv =haskellPackages.callCabal2nix "netlink-pm" ./. {}; 
# # haskellPackages.developPackage { root = ./.; }
# in
#   if pkgs.lib.inNixShell then drv.env else drv
