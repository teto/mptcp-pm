{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, netlink, stdenv, ghc-mod, hlint, hdevtools }:
      mkDerivation {
        pname = "netlink-pm";
        version = "1.0.0";
        src = ./hs;
        isLibrary = false;
        isExecutable = true;
        # by default ghc-mod, hlint, and hdevtools
        # 
        # ghc-mod fait foirer la compilation
        executableHaskellDepends = [ base netlink hlint hdevtools ];
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
