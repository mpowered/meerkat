{ compiler ? "ghc843" }:

let
  nixpkgs = import (builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz;
    sha256 = "19kryzx9a6x68mpyxks3dajraf92hkbnw1zf952k73s2k4qw9jlq";
  }) {};

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages."${compiler}";

  drv = haskellPackages.callCabal2nix "meerkat" ./. {};

in
{
  meerkat = drv;
  meerkat-shell = haskellPackages.shellFor {
    packages = p: [drv];
  };
}
