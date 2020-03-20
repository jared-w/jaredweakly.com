{ pkgs ? import <nixpkgs> { } }:
let
  nodePkgs = (with pkgs.nodePackages; [ html-minifier serve ]);
  inherit (import (builtins.fetchTarball
    "https://github.com/hercules-ci/gitignore/archive/master.tar.gz") { })
    gitignoreSource;
in pkgs.haskellPackages.developPackage {
  name = "jaredweakly";
  root = gitignoreSource ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [ cabal-install ghcid brittany ] ++ nodePkgs
      ++ (with pkgs; [ fsatrace just watchexec nodejs_latest ]));
}
