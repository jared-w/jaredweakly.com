let
  pkgs = import (fetchTarball
    "https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz")
    { };
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
    sha256 = "sha256:0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
in let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          site = pkgs.haskell.lib.addExtraLibraries
            (self.callCabal2nix "jaredweakly" (gitignoreSource ./.) { })
            (with pkgs.nodePackages; [ html-minifier serve ]);
        };
      };
    };
  };
  pkgs = import (fetchTarball
    "https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz") {
      inherit config;
    };
in if pkgs.lib.inNixShell then
  pkgs.haskellPackages.site.env
else {
  site = pkgs.haskellPackages.site;
}
