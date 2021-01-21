{ sources ? import ./sources.nix, system ? builtins.currentSystem }:
let
  srcs = _: _: { inherit sources; };
  overlays = [ srcs ];
in import sources.nixpkgs { inherit overlays system; }
