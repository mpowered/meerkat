{ compiler ? "ghc864" }:

let
  nixpkgs = import (builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.09.tar.gz;
  }) {};

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages."${compiler}";

  drv = haskellPackages.callCabal2nix "meerkat" ./. {
    beam-postgres = with pkgs.haskell.lib; unmarkBroken (dontCheck haskellPackages.beam-postgres);
  };

in
{
  meerkat = drv;
  meerkat-shell = haskellPackages.shellFor {
    packages = p: [drv];
  };
}
