
This is a rewrite in haskell of the python netlink module.
nix-shell -p 'haskellPackages.ghcWithHoogle(p: with p; [netlink optparse-applicative ])'


The netlink module asks for GENL_ADMIN_PERM => The operation requires the CAP_NET_ADMIN privilege

sudo setcap cap_net_admin+ep hs/dist-newstyle/build/x86_64-linux/ghc-8.6.3/netlink-pm-1.0.0/x/daemon/build/daemon/daemon

# Netlink explanation

To fetch TCP diagnostics:
Creates a socket with family eNETLINK_INET_DIAG (really NETLINK_SOCK_DIAG) with value 4
AF_INET => netlink family 2



# Compilation

With a custom netlink and kernel
Compile the custom netlink library with
```
$ cabal configure --enable-library-profiling
```
```
kernel $ make headers_install
$ cabal configure --package-db /home/teto/netlink-hs/dist/package.conf.inplace --extra-include-dirs=/home/teto/mptcp2/build/usr/include -v3 --enable-profiling
```


# Usage

Enter the nix-shell shell-test.nix and start the daemon:

$ cabal run daemon

or
$ buildNRun
To print a stacktrace
cabal run daemon toto -- +RTS -xc

In a shell:
`$ nix run nixpkgs.iperf -c iperf -s`

In another:
`$ nix run nixpkgs.iperf -c iperf -c localhost -b 1KiB -t 4 --cport 5500 -4`

TODO:
ss package sends by default
```
-- #define SS_ALL ((1 << SS_MAX) - 1)
-- #define SS_CONN (SS_ALL & ~((1<<SS_LISTEN)|(1<<SS_CLOSE)|(1<<SS_TIME_WAIT)|(1<<SS_SYN_RECV)))
-- #define TIPC_SS_CONN ((1<<SS_ESTABLISHED)|(1<<SS_LISTEN)|(1<<SS_CLOSE))
```
- [ ] write wordToEnums function, especially to fix getSockDiagRequestHeader
(with bitset package once it's fixed)

# Testsuite

# BUGS

- conversion of IDiagExt is bad everywhere ? req.r.idiag_ext |= (1<<(INET_DIAG_INFO-1));
- we need to request more states

# TODO 
- pass local/server IPs as commands to the PM ?
- generate completion scripts via --zsh-completion-script
- to get kernel ifindex: cat /sys/class/net/lo/ifindex
