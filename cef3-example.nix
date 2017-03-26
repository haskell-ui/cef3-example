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
  buildTools = [ wget ];
  postInstall = ''
    wget --no-check-certificate https://raw.githubusercontent.com/Semantic-Org/Semantic-UI-CSS/7ad02f2fb9e50087fcb02bc1fba05e900488381c/semantic.min.css
    mkdir -p $out/bin/static/css
    mv semantic.min.css $out/bin/static/css/
    ln -s ${libcef3}/bin/locales $out/bin/
  '';
}
