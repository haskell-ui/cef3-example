{ mkDerivation, base, cef3-simple, stdenv, libcef3
, threepenny-gui, optparse-applicative }:
mkDerivation {
  pname = "cef3-example";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
      base cef3-simple threepenny-gui
      optparse-applicative
    ];
  description = "CEF3 bindings usage example";
  license = stdenv.lib.licenses.bsd3;
  postInstall = ''
    ln -s ${libcef3}/bin/locales $out/bin/
  '';
}
