let
  pkgs = import ./nix { };
  py = pkgs.python3.withPackages (p: [ p.fonttools p.brotli p.zopfli ]);
  haskellPkgs = with pkgs.haskell.packages.ghc884; [
    zlib
    cabal-install
    haskell-language-server
    fourmolu
  ];
  buildTools = with pkgs; [ caddy fsatrace just watchexec nodejs_latest niv ];
in pkgs.mkShell {
  buildInputs = haskellPkgs ++ buildTools ++ [ py pkgs.zlib.all pkgs.xz.all ];
}
