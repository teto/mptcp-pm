# to use with nixos-shell
{ pkgs, lib, ... }:
let
  userNixpkgs = /home/teto/nixpkgs;
  customOverlay = /home/teto/home;
  vlans = [ 0 1 ];
in
{
  # imports = [
  #   /home/teto/dotfiles/nixpkgs/account-teto.nix
  # ];

  # cmdline="root=/dev/sda1 earlycon=ttyS0 console=ttyS0 init=/nix/var/nix/profiles/system/init boot.debug=1 boot.consoleLogLevel=1 nokaslr tcp_probe.port=5201 tcp_probe.full=1";
  # # # x86_64 is a symlink towards x86
  # kernel="/home/teto/mptcp/build/arch/x86_64/boot/bzImage";

  # services.mingetty.helpLine = "[9;0][14;0]";

  # Will add an eth
  virtualisation.vlans = vlans;
  # lib.mkForce 
  virtualisation.qemu.networkingOptions = [ ];


  # slirpvde -s vde0.ctl -dhcp
  # one can 
  environment.systemPackages = with pkgs; [
    neovim
    dhcp # allows to run dhclient
    # mptcpnumerics  # from my overlay ? copy it here
  ];

  services.xserver.enable = false;
  
  networking = {
    useDHCP = false;
      localCommands = ''
        echo "HELLO YOU";
      '';
  };

  # "nixos=https://github.com/nixos/nixpkgs-channels/archive/nixos-19.03.tar.gz"
  nix = {
    nixPath = [
      "nixos-unstable=https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz"
    ]
    ++ pkgs.stdenv.lib.optional (builtins.pathExists userNixpkgs)  "nixpkgs=${builtins.toString userNixpkgs}";
  };

  networking.mptcp.enable = true;

  # mountNix
  # nixos-shell.mounts.extraMounts = {

  #   # "/mnt/examples" = ./.;

  #   "/mnt/nixos-shell" = {
  #     target = ./..;
  #     cache = "none";
  #   };
  # };
}
