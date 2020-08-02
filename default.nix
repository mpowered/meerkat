let
  sources = import ./nix/sources.nix;
in
  { haskellNix  ? import sources.haskell-nix {}
  , nixpkgsSrc  ? haskellNix.sources.nixpkgs-2003
  , nixpkgsArgs ? haskellNix.nixpkgsArgs
  , pkgs        ? import nixpkgsSrc nixpkgsArgs
  }:

    pkgs.haskell-nix.project {
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "meerkat";
        src = ./.;
      };
      compiler-nix-name = "ghc865";
    }
