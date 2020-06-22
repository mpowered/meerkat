let
  sources = import ./nix/sources.nix;
  haskellNix = import sources.haskell-nix {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  pkgs = import nixpkgsSrc nixpkgsArgs;
in
  pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "meerkat";
      src = ./.;
    };
  }
