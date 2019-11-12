let
 config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          site =
            haskellPackagesNew.callPackage ./jaredweakly.nix { };
        };
      };
    };
  };
  pkgs = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) { inherit config; };
in
  { site = pkgs.haskellPackages.site; }
