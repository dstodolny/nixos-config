{ stdenv, fetchgit, pkgconfig, writeText, libX11, conf ? null }:

with stdenv.lib;

stdenv.mkDerivation rec {
  name = "slstatus-${version}";
  version = "unstable-2018-04-16";

  src = fetchgit {
    url = "https://github.com/dnixty/slstatus";
    rev = "9b5cbc9838c4fc332bf594539ccdccc3b084617c";
    sha256 = "1i2x30qzb3ih0kj0jxaqlx66y8qphzajc2c4iwm3q5yvmfh1v5w0";
  };

  configFile = optionalString (conf!=null) (writeText "config.def.h" conf);
  preBuild = optionalString (conf!=null) "cp ${configFile} config.def.h";

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ libX11 ];

  installFlags = [ "PREFIX=$(out)" ];

  meta = {
    homepage = https://tools.suckless.org/slstatus/;
    description = "status monitor for window managers that use WM_NAME like dwm";
    license = licenses.isc;
    maintainers = with maintainers; [ geistesk ];
    platforms = platforms.linux;
  };
}
