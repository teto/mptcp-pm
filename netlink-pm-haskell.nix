{ mkDerivation, stack, base, netlink, stdenv }:
mkDerivation {
  pname = "netlink-pm";
  version = "1.0.0";
  src = ./hs;
  isLibrary = false;
  isExecutable = true;
  # libraryHaskellDepends = [ ];
  executableHaskellDepends = [ base netlink stack ];
  # testHaskellDepends = [ ];
  # homepage= 
  # description
  # license
  license = stdenv.lib.licenses.gpl3;
}
