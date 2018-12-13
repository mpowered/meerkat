{ compiler ? "ghc844" }:
(import ./. { inherit compiler; }).meerkat-shell
