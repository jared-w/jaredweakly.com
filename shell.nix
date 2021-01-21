let
  pkgs = import ./nix { };
  nodePkgs = with pkgs.nodePackages; [ html-minifier serve ];
  haskellPkgs = with pkgs.haskellPackages; [
    cabal-install
    haskell-language-server
    fourmolu
    hpack
  ];
  buildTools = with pkgs; [ fsatrace just watchexec nodejs_latest niv ];
  ps = haskellPkgs ++ nodePkgs ++ buildTools;
  inherit (import (pkgs.sources.gitignore) { }) gitignoreSource;
in pkgs.haskellPackages.developPackage {
  name = "jaredweakly";
  root = gitignoreSource ./.;
  modifier = drv: pkgs.haskell.lib.addBuildTools drv ps;
}
