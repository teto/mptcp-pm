{ pkgs ? import <nixpkgs> {} }:

pkgs.pythonPackages.callPackage ./default.nix {}
