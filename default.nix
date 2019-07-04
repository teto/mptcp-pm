{ compiler }:
with import <nixpkgs> {};

# enableSeparateDocOutput
haskell.lib.doHaddock (compiler.callCabal2nix "mptcp-pm" ./. {})

# let
#   drv =haskellPackages.callCabal2nix "netlink-pm" ./. {}; 
# # haskellPackages.developPackage { root = ./.; }
# in
#   if pkgs.lib.inNixShell then drv.env else drv
