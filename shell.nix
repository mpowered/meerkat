let
  hsPkgs = import ./default.nix;
in
  hsPkgs.shellFor {
    packages = ps: [
      ps.meerkat
    ];

    exactDeps = true;
  }
