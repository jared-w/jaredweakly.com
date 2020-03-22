{ sources ? import ./sources.nix, system ? builtins.currentSystem }:
let
  niv = _: _: { inherit (import sources.niv { }) niv; };
  snack = _: _: { snack = (import sources.snack).snack-exe; };
  srcs = _: _: { inherit sources; };
  overlays = [ srcs niv snack ];
in import sources.nixpkgs { inherit overlays system; }
