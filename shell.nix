let
  sources = import ./nix/sources.nix;
in
  { haskellNix  ? import sources.haskell-nix {}
  , nixpkgsSrc  ? haskellNix.sources.nixpkgs-2003
  , nixpkgsArgs ? haskellNix.nixpkgsArgs
  , pkgs        ? import nixpkgsSrc nixpkgsArgs
  }:

    let
      hsPkgs = import ./default.nix { inherit haskellNix nixpkgsSrc nixpkgsArgs pkgs; };
    in
      hsPkgs.shellFor {
        packages = ps: [
          ps.meerkat
        ];

        withHoogle = true;
        tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; ghcide = "0.2.0"; };
        exactDeps = true;
      }
