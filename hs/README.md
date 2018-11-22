
This is a rewrite in haskell of the python netlink module.
nix-shell -p 'haskellPackages.ghcWithHoogle(p: with p; [netlink optparse-applicative ])'
