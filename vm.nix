# to use with nixos-shell https://github.com/Mic92/nixos-shell
# jjjjjj
# $QEMU_OPTS
# TODO: this could also
{ pkgs, lib, ... }:
let
  userNixpkgs = /home/teto/nixpkgs;
  customOverlay = /home/teto/home;
  vlans = [ 0 1 ];

  # new style can compact -device and -netdev into -nic
  # qemuNICFlags = nic: net: machine:
  # [
  #     "-nic user,model=virtio-net-pci,id=vlan${toString nic}"
  # ];

in
{
  # TODO pass to
  # imports = [
  #   # /home/teto/dotfiles/nixpkgs/account-teto.nix
  # ];

  # cmdline="root=/dev/sda1 earlycon=ttyS0 console=ttyS0 init=/nix/var/nix/profiles/system/init boot.debug=1 boot.consoleLogLevel=1 nokaslr tcp_probe.port=5201 tcp_probe.full=1";
  # # # x86_64 is a symlink towards x86
  # kernel="/home/teto/mptcp/build/arch/x86_64/boot/bzImage";

  # Will add an eth
  virtualisation.vlans = vlans;
  virtualisation.qemu.networkingOptions = lib.mkForce [ ];

  # boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = true;

  environment.systemPackages = with pkgs; [
    neovim
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
