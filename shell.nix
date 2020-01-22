{ pkgs ? import <nixpkgs> { } }:
let nodePkgs = (with pkgs.nodePackages; [ html-minifier serve ]);
in pkgs.haskellPackages.developPackage {
  name = "jaredweakly";
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [ cabal-install ghcid brittany ] ++ nodePkgs ++ [ pkgs.fsatrace ]);
}
