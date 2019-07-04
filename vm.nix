# to use with nixos-shell https://github.com/Mic92/nixos-shell
# jjjjjj
# $QEMU_OPTS
# TODO: this could also
{ pkgs, ... }:
let
  userNixpkgs = /home/teto/nixpkgs;
  customOverlay = /home/teto/home;
in
{
  # TODO pass to
  imports = [
    /home/teto/dotfiles/nixpkgs/account-teto.nix
  ];

      # cmdline="root=/dev/sda1 earlycon=ttyS0 console=ttyS0 init=/nix/var/nix/profiles/system/init boot.debug=1 boot.consoleLogLevel=1 nokaslr tcp_probe.port=5201 tcp_probe.full=1";
      # # # x86_64 is a symlink towards x86
      # kernel="/home/teto/mptcp/build/arch/x86_64/boot/bzImage";

  # Will add an eth
  virtualisation.vlans = [ 2 1 3 4 ];

  environment.systemPackages = with pkgs; [
    mptcpanalyzer  # from my overlay ? copy it here
  ];

  #       cmdline="root=/dev/sda1 earlycon=ttyS0 console=ttyS0 init=/nix/var/nix/profiles/system/init boot.debug=1 raid=noautodetect nokaslr
  virtualisation.qemu.options = [
    "-kernel /home/teto/mptcp/build/arch/x86_64/boot/bzImage"
    # boot.consoleLogLevel=1
    ''-cmdline root=/dev/sda1 earlycon=ttyS0 console=ttyS0 init=/nix/var/nix/profiles/system/init boot.debug=1  nokaslr tcp_probe.port=5201 tcp_probe.full=1"''
  ];

  # TODO load up an mptcp module

  # networking.interfaces.ens192 = secrets.gitolite_server.interfaces;
    # interfaces = {
    #   ipv4.addresses = [ { address = "202.214.86.51"; prefixLength = 25; } ];
    #   ipv6.addresses = [ { address = "2001:240:168:1001::37"; prefixLength = 25; } ];
    # };

  # hostname = "mptcp.iijlab.net";

  # "nixos=https://github.com/nixos/nixpkgs-channels/archive/nixos-19.03.tar.gz"
  nix = {
    nixPath = [
      "nixos-unstable=https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz"
    ]
    ++ pkgs.stdenv.lib.optional (builtins.pathExists userNixpkgs)  "nixpkgs=${builtins.toString userNixpkgs}";
  };

  # mountNix
  nixos-shell.mounts.extraMounts = {

    "/mnt/examples" = ./.;

    "/mnt/nixos-shell" = {
      target = ./..;
      cache = "none";
    };
  };
}
