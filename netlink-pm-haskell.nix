{ mkDerivation, stack, base, netlink, stdenv
, ghc-mod, hindent, hlint
}:
mkDerivation {
  pname = "netlink-pm";
  version = "1.0.0";
  src = ./hs;
  isLibrary = false;
  isExecutable = true;
  # libraryHaskellDepends = [ ];
  executableHaskellDepends = [ base netlink 
    # stack
  ];

    #overrides = self: super:
    #  { # Don't run a package's test suite
    #    # foo = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.foo;
    #    #
    #    # Don't enforce package's version constraints
    #    bar = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.cabal-helper;
    #    #
    #    # To discover more functions that can be used to modify haskell
    #    # packages, run "nix-repl", type "pkgs.haskell.lib.", then hit
    #    # <TAB> to get a tab-completed list of functions.
    #  };

  # 
  buildDepends = [
    ghc-mod 
    hindent 
    hlint 
  ];
  # testHaskellDepends = [ ];
  # homepage= 
  # description
  # license
  license = stdenv.lib.licenses.gpl3;
}
