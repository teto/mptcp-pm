[![Hackage][hk-img]][hk]

This is a userspace path manager for the [linux multipath TCP
kernel][mptcp-fork], starting from version v0.95.

This allows to monitor MPTCP connections and control what subflows to create and
with a custom kernel it can even set specific values for the congestion windows.


# Compilation

For now we need a custom version of netlink
With a custom netlink and kernel
Compile the custom netlink library with
```
$ cabal configure --enable-library-profiling
```
You may need some headers as well (NOTE: reference cabal.project instead):
```
kernel $ make headers_install
$ cabal configure --package-db ~/netlink-hs/dist/package.conf.inplace --extra-include-dirs=~/mptcp/build/usr/include -v3 --enable-profiling
```

To compile the doc (and understand why HLS fails displaying anything)
`cabal haddock --all`

# Usage

The netlink module asks for `GENL_ADMIN_PERM` which requires the `CAP_NET_ADMIN` privilege.
You can assign this privilege via:

```
sudo setcap cap_net_admin+ep hs/dist-newstyle/build/x86_64-linux/ghc-8.6.3/netlink-pm-1.0.0/x/daemon/build/daemon/daemon
```

Enter the development shell and start the daemon:

```
$ nix develop
$ cabal run mptcp-manager
```

# TODO
- remove the need for MptcpSocket everywhere: it's just needed to write the
header, which could be added/modifier later instead ! (to increase purity in the
    library)
- we need to better keep track of subflow status (established vs WIP) ?
- pass local/server IPs as commands to the PM ?
- generate completion scripts via --zsh-completion-script


# Acknowledgements

This work is sponsored by [NGI][ngi].

[hk-img]: https://img.shields.io/hackage/v/mptcp-pm.svg?logo=haskell
[hk]: https://hackage.haskell.org/package/mptcp-pm
[mptcp-fork]: http://multipath-tcp.org/
[ngi]: https://www.ngi.eu/
