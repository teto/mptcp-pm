{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "joyofhaskell.com";
  inherit ghc;
  buildInputs = with pkgs; [ sassc zlib ];
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}
