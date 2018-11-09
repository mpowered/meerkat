{ compiler ? "ghc843" }:
(import ./. { inherit compiler; }).meerkat-shell
