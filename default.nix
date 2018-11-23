{ compiler ? "ghc843" }:

let
  nixpkgs = import (builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz;
    sha256 = "1jrzibmq3m74ivwf49nklnrm6prk1ifk4g29jwzdv6yrcfk9bhvv";
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
