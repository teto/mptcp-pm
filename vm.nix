# to use with nixos-shell https://github.com/Mic92/nixos-shell
{ pkgs, ... }: {

  virtualisation.vlans = [ 2 1 ];

  mountNix
  nixos-shell.mounts.extraMounts = {

    "/mnt/examples" = ./.;

    "/mnt/nixos-shell" = {
      target = ./..;
      cache = "none";
    };
  };
}
