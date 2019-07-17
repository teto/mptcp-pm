# to use with nixos-shell
{ pkgs, lib, ... }:
let
  userNixpkgs = /home/teto/nixpkgs;
  customOverlay = /home/teto/home;
  vlans = [ 0 1 ];

in
{
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
