{ compiler ? "ghc865" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  haskellPackages = pkgs.haskell.packages."${compiler}";

  drv = haskellPackages.callCabal2nix "meerkat" ./. {
    beam-core = with pkgs.haskell.lib; unmarkBroken (dontCheck haskellPackages.beam-core);
    beam-postgres = with pkgs.haskell.lib; unmarkBroken (dontCheck haskellPackages.beam-postgres);
    hedis = with pkgs.haskell.lib; dontCheck (haskellPackages.callCabal2nix "hedis" sources.hedis {});
  };

in
{
  meerkat = drv;
  meerkat-shell = haskellPackages.shellFor {
    packages = p: [drv];
  };
}
