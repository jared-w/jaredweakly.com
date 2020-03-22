let
  pkgs = import ./nix { };
  nodePkgs = (with pkgs.nodePackages; [ html-minifier serve ]);
  inherit (import (pkgs.sources.gitignore) { }) gitignoreSource;
in pkgs.haskellPackages.developPackage {
  name = "jaredweakly";
  root = gitignoreSource ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [ cabal-install ghcid brittany hpack ] ++ nodePkgs
      ++ (with pkgs; [ fsatrace just watchexec nodejs_latest niv ]));
}
