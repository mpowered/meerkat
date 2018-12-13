{ compiler ? "ghc844" }:

let
  nixpkgs = import (builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz;
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
