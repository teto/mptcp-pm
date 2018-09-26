mptcpnetlink
============


Follow up of repository http://github.com/teto/mptcpnetlink:
hosts 2 components that allow to control from userspace the MPTCP kernel (http://multipath-tcp.org) path management system:





### Current state

The current module only propagates new MPTCP sessions events (__new_session__ callback) but it should be pretty easy to extend this to other events.

### REQUIREMENTS

- a correctly patched kernel 
- for the python daemon, you need this custom version of libnl (userspace netlink library) that improves the python bindings: https://github.com/teto/libnl_old 


### COMPILATION

3. Compilation of libnl:

```
$ git clone https://github.com/teto/libnl_old libnl && cd libnl
libnl$ ./autogen.sh
libnl$ ./configure
libnl$ make 
libnl$ make install
libnl$ cd python
python# python3 setup.py build
python# python3 setup.py install --user # will install just for your current
user
```
Optional: In case you modify the python code without modifying the bindings, you can also add the libnl/python/build/linux to the PYTHONPATH

### HOW TO USE (once compiled/installed) ?

1. First register the kernel module:
`sysctl -w net.mptcp.mptcp_path_manager="netlink"`

2. Load the python module with the number of subflows to create:
`daemon/daemon.py --simulate 2`
The "--simulate" flag is used to bypass the default system which is described in our paper (lig retrieves the number of LISP paths)



### How to debug netlink ?
Run `genl ctrl list` to list
http://0x90.at/post/netlink-debugging
https://jvns.ca/blog/2017/09/03/debugging-netlink-requests/

### Haskell experiment

nix-shell shell-haskell.nix


For now launch with nix-shell -p 'haskellPackages.ghcWithHoogle(p: with p; [netlink optparse-applicative])'
