
This is a rewrite in haskell of the python netlink module.
nix-shell -p 'haskellPackages.ghcWithHoogle(p: with p; [netlink optparse-applicative ])'


The netlink module asks for GENL_ADMIN_PERM => The operation requires the CAP_NET_ADMIN privilege

sudo setcap cap_net_admin+ep hs/dist-newstyle/build/x86_64-linux/ghc-8.6.3/netlink-pm-1.0.0/x/daemon/build/daemon/daemon

# Compilation

With a custom netlink:
$ cabal configure --package-db /home/teto/netlink-hs/dist/package.conf.inplace -v


Testing procedure:

Enter the nix-shell shell-test.nix and start the daemon:
$ buildNRun

In a shell:
$ nix run nixpkgs.iperf -c iperf -s

In another
$ nix run nixpkgs.iperf -c iperf -c localhost -b 1KiB -t 4 --cport 5500

TODO:
- [ ] generate the enums via FFI from include/uapi/linux/mptcp.h
