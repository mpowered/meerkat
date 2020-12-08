let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { inherit config; };
  compilerVersion = "ghc8102";
  compilerSet = pkgs.haskell.packages."${compilerVersion}";
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = with pkgs.haskell.lib; self: super: {
          meerkat = super.callCabal2nix "meerkat" (gitIgnore [./.gitignore] ./.) {};
          beam-core = markUnbroken (appendPatch super.beam-core ./patches/beam-core-aeson.patch);
          beam-postgres = markUnbroken (appendPatch super.beam-postgres ./patches/beam-postgres-aeson.patch);
          beam-migrate = markUnbroken (appendPatch super.beam-migrate ./patches/beam-migrate-aeson.patch);
          tmp-postgres = markUnbroken (dontCheck super.tmp-postgres);
        };
      };
    };
  };

in {
  inherit pkgs;
  shell = compilerSet.shellFor {
    packages = p: [ p.meerkat ];
    buildInputs = with pkgs; [
      compilerSet.cabal-install
      compilerSet.haskell-language-server
    ];
  };
}
