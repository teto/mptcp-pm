
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


To compile the doc (and understand why HIE fails displaying anything)
`cabal haddock --all`

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


Script to reload module


```
reload_mod() {
    newMod="$1"

	if [ -z "${newMod}" ]; then
		echo "Use: <path to new module>"
		echo "possibly /home/teto/mptcp2/build/net/mptcp/mptcp_netlink.ko"
	fi
# 1. change to another scheduler
    mppm "fullmesh"
	sleep 1
# 2. rmmod the current one
    sudo rmmod "mptcp_netlink"

# 3. Insert our new module
	sleep 1
    sudo insmod "$1"

# 4. restore path manager
	mppm "netlink"

}
```


# Testsuite

# BUGS
- conversion of SockDiagExtensionId is bad everywhere ? req.r.idiag_ext |= (1<<(INET_DIAG_INFO-1));
- we need to request more states

# TODO
- remove the need for MptcpSocket everywhere: it's just needed to write the
header, which could be added/modifier later instead ! (to increase purity in the
    library)
- replace Enum2Bits wordToEnums function, especially to fix getSockDiagRequestHeader
(with bitset package once it's fixed)
- we need to better keep track of subflow status (established vs WIP) ?
- pass local/server IPs as commands to the PM ?
- generate completion scripts via --zsh-completion-script


Note:
ss package sends by default
-- #define SS_ALL ((1 << SS_MAX) - 1)
-- #define SS_CONN (SS_ALL & ~((1<<SS_LISTEN)|(1<<SS_CLOSE)|(1<<SS_TIME_WAIT)|(1<<SS_SYN_RECV)))
-- #define TIPC_SS_CONN ((1<<SS_ESTABLISHED)|(1<<SS_LISTEN)|(1<<SS_CLOSE))



