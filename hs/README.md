
This is a rewrite in haskell of the python netlink module.
nix-shell -p 'haskellPackages.ghcWithHoogle(p: with p; [netlink optparse-applicative ])'


The netlink module asks for GENL_ADMIN_PERM => The operation requires the CAP_NET_ADMIN privilege

sudo setcap cap_net_admin+ep hs/dist-newstyle/build/x86_64-linux/ghc-8.6.3/netlink-pm-1.0.0/x/daemon/build/daemon/daemon


TEMP

#!/usr/bin/env bash
exec cabal -v0 --project-file=path/to/project/cabal.project new-run exe:exe-name -- "$@"
