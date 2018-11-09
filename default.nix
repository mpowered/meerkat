{ compiler ? "ghc843" }:

let
  nixpkgs = import (builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz;
    sha256 = "1lagfycy2lvfc8cdxk98dz2rxjlrbmv9hj42x0x40sy66bck1w0y";
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
