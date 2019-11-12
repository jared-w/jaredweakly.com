{ mkDerivation, aeson, base, containers, hasmin, lens, lens-aeson
, pandoc, pandoc-emphasize-code, pandoc-types, process, shake
, slick, stdenv, text, time, unordered-containers
}:
mkDerivation {
  pname = "jaredweakly";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers hasmin lens lens-aeson pandoc
    pandoc-emphasize-code pandoc-types process shake slick text time
    unordered-containers
  ];
  homepage = "https://github.com/jared-w/jaredweakly.com#readme";
  description = "My website";
  license = stdenv.lib.licenses.mit;
}
