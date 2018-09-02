{ mkDerivation, stack, base, netlink, stdenv
, ghc-mod, hindent, hlint
, Cabal
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

  license = stdenv.lib.licenses.gpl3;
}
