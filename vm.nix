# to use with nixos-shell https://github.com/Mic92/nixos-shell
# jjjjjj
# $QEMU_OPTS
# TODO: this could also
{ pkgs, ... }:
let
  userNixpkgs = /home/teto/nixpkgs;
  customOverlay = /home/teto/home;
  vlans = [ 2 1 ];

  # Extracted from qemu-flags 
  # to be able to set 
  # with import <nixpkgs/nixos/lib/qemu-flags.nix> { inherit pkgs; };

  # pad to get at least 2 digits
  zeroPad = n: if n < 10 then "0${toString n}" else toString n;

  # le defaut c'est  
  # -net nic,netdev=user.0,model=virtio -netdev user,id=user.0${QEMU_NET_OPTS:+,$QEMU_NET_OPTS} \
  # ,sock=$QEMU_VDE_SOCKET_${toString net}"
  # see https://wiki.qemu.org/Documentation/Networking 's doc
  # we need on -netdev 
  # -device e1000
  # -nic user

  # qemuNICFlags = nic: net: machine:
  # [
  #     "-device virtio-net-pci,netdev=vlan${toString nic},mac=52:54:00:12:${zeroPad net}:${zeroPad machine}"
  #     "-netdev user,id=vlan${toString nic},net=192.168.76.${toString nic}/24"
  # ];


  # new style can compact -device and -netdev into -nic
  # qemuNICFlags = nic: net: machine:
  # [
  #     "-nic user,model=virtio-net-pci,id=vlan${toString nic}"
  # ];

    # need to pass net=192.168.76.0/24,
in
{
  # TODO pass to
  imports = [
    # /home/teto/dotfiles/nixpkgs/account-teto.nix

  ];

  # cmdline="root=/dev/sda1 earlycon=ttyS0 console=ttyS0 init=/nix/var/nix/profiles/system/init boot.debug=1 boot.consoleLogLevel=1 nokaslr tcp_probe.port=5201 tcp_probe.full=1";
  # # # x86_64 is a symlink towards x86
  # kernel="/home/teto/mptcp/build/arch/x86_64/boot/bzImage";

  # Will add an eth
  virtualisation.vlans = vlans;

  # boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = true;

  environment.systemPackages = with pkgs; [
    # mptcpnumerics  # from my overlay ? copy it here
  ];

  # boot.initrd.supportedFilesystems
  # /mnt-root//nix/var/nix/profiles/system/init
  # cmdline="root=/dev/sda1 earlycon=ttyS0 console=ttyS0 init=/nix/var/nix/profiles/system/init boot.debug=1 raid=noautodetect nokaslr
  # virtualisation.qemu.options =
    # [
    # "-kernel /home/teto/mptcp2/build/arch/x86_64/boot/bzImage"
    # boot.consoleLogLevel=1
    # ''-append "root=/dev/sda1 earlycon=ttyS0 console=ttyS0 init=/nix/var/nix/profiles/system/init boot.debug=1  nokaslr tcp_probe.port=5201 tcp_probe.full=1"''
    # si j'utilise mon propre kernel g besoin de mon propre initramfs
    # "-initrd "
  # ];

  # -m must be a machine
  # qemuNICFlags = nic: net: machine: because 
  # virtualisation.qemu.options doit etre une liste
  # zipLists [ 1 2 ] [ "a" "b" ]
  # => [ { fst = 1; snd = "a"; } { fst = 2; snd = "b"; } ]

  # done automatically by assigned address

  # virtualisation.qemu.options = with pkgs.lib; let
  #             m = {snd = 1;};
  #             interfacesNumbered = zipLists vlans (range 1 255);
  #             # interfaces = flip map interfacesNumbered ({ fst, snd }:
  #             #   nameValuePair "eth${toString snd}" { ipv4.addresses =
  #             #     [ { address = "192.168.${toString fst}.${toString m.snd}";
  #             #         prefixLength = 24;
  #             #     } ];
  #             #   });
  #   in
  #     flip map interfacesNumbered
  #       ({ fst, snd }: qemuNICFlags snd fst m.snd);

  # TODO load up an mptcp module

  # networking.interfaces.ens192 = secrets.gitolite_server.interfaces;
    # interfaces = {
    #   ipv4.addresses = [ { address = "202.214.86.51"; prefixLength = 25; } ];
    #   ipv6.addresses = [ { address = "2001:240:168:1001::37"; prefixLength = 25; } ];
    # };

  # networking.hostName = "netlink";

  services.xserver.enable = false;

  # "nixos=https://github.com/nixos/nixpkgs-channels/archive/nixos-19.03.tar.gz"
  nix = {
    nixPath = [
      "nixos-unstable=https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz"
    ]
    ++ pkgs.stdenv.lib.optional (builtins.pathExists userNixpkgs)  "nixpkgs=${builtins.toString userNixpkgs}";
  };

  # mountNix
  nixos-shell.mounts.extraMounts = {

    # "/mnt/examples" = ./.;

    "/mnt/nixos-shell" = {
      target = ./..;
      cache = "none";
    };
  };
}
