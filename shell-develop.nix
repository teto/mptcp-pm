
# https://github.com/peti/nixpkgs/tree/haskell-updates
# 6f916e5209155c89c273ac08a242c058a95404b0 seems to work well
# "https://github.com/NixOS/nixpkgs/archive/<nixpkgs_commit_hash>.tar.gz"
{ pkgs ? import <nixpkgs> {} }: 
# { pkgs ? import (builtins.fetchTarball "https://github.com/peti/nixpkgs/archive/6f916e5209155c89c273ac08a242c058a95404b0.tar.gz" ) {} }: 

let 
  
  devPkg = pkgs.haskellPackages.developPackage {
  root = ./hs;
  overrides = self: super:
    { # Don't run a package's test suite
      # foo = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.foo;
      #
      # Don't enforce package's version constraints
      # bar = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.bar;
      
      # StateVar = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.StateVar;

      #
      # To discover more functions that can be used to modify haskell
      # packages, run "nix-repl", type "pkgs.haskell.lib.", then hit
      # <TAB> to get a tab-completed list of functions.
    };
  #source-overrides =
  #  { # Use a specific hackage version
  #    # optparse-applicative = "0.14.0.0";
  #    #
  #    # Use a particular commit from github
  #    # my-private-package = pkgs.fetchFromGitHub
  #    #   { owner = "my-github-username";
  #    #     repo = "my-private-package";
  #    #     rev = "561de381bcccfe6792f2908a5022449a05ae0050";
  #    #     sha256 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
  #    #   };
  #  };

};
in 
  #nix-shell -p 'haskell.packages.ghc821.ghcWithPackages (p: with p; [ghc-mod hlint (haskellPackages.developPackage { root = ./.; })])' -j4 --run 'zsh
  # devPkg

  pkgs.haskellPackages.ghcWithPackages 
  (p: with p; [ghc-mod hlint devPkg])

   # pkg.overrideAttrs(attr: {
   #   # add cabal ?
   #   buildInputs = [zlib];
   # })
