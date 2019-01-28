with import <nixpkgs> { };
let
  language-puppet_git = pkgs.haskellPackages.callCabal2nix "language-puppet" (pkgs.fetchFromGitHub {
    owner  = "bartavelle";
    repo   = "language-puppet";
    rev    = "22d8fb4ee0ad833ca2f0790e3c473e7e49424232";
    sha256 = "1llzhf48r30qnanaay6r6mdvq4pw0ba200pl0pfagfg2kja54a9v";
  }) {};
  henv = pkgs.haskellPackages.ghcWithPackages (p: with p; [language-puppet_git ]);
in
pkgs.stdenv.mkDerivation {
  name = "build-env";
  buildInputs = [ henv ];
}
