{ mkDerivation, stack, base, netlink, stdenv }:
mkDerivation {
  pname = "netlink-pm";
  version = "1.0.0";
  src = ./hs;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base netlink stack ];
  license = stdenv.lib.licenses.gpl3;
}
