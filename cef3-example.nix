{ mkDerivation, base, cef3-simple, stdenv, libcef3, split
, colour, threepenny-gui, optparse-applicative, protolude
, filepath, wget }:
mkDerivation {
  pname = "cef3-example";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
      base cef3-simple threepenny-gui colour
      optparse-applicative protolude split filepath
    ];
  description = "CEF3 bindings usage example";
  license = stdenv.lib.licenses.bsd3;
  postInstall = ''
    mkdir -p $out/bin/static/css
    mv resources/semantic.min.css $out/bin/static/css/
    ln -s ${libcef3}/bin/locales $out/bin/
  '';
}
