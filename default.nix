{ compiler ? "ghc843" }:

let
  nixpkgs = import (builtins.fetchGit {
    url = https://github.com/NixOS/nixpkgs-channels;
    ref = "nixos-18.09";
  }) {};

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages."${compiler}".override {
    overrides = self: super: {
      # meerkat = self.callPackage ./. {};
    };
  };

  drv = haskellPackages.callCabal2nix "meerkat" ./. {};

in
{
  meerkat = drv;
  meerkat-shell = haskellPackages.shellFor {
    packages = p: [drv];
  };
}
