{ compiler ? "ghc865" }:
(import ./. { inherit compiler; }).meerkat-shell
