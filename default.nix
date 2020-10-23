{ pkgs ? import <nixpkgs> {} }: with pkgs; let
  ghc = haskellPackages.ghcWithPackages (p: with p; [
    random
    gloss
  ]);
in stdenv.mkDerivation {
  name = "hsnet";
  src = ./.;
  nativeBuildInputs = [ ghc ];
}

