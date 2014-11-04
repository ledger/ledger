{ stdenv, fetchgit, cmake, boost, gmp, mpfr, libedit, python
, texinfo, gnused }:

let
  rev = "20141005";
in

stdenv.mkDerivation {
  name = "ledger-3.1.0.${rev}";
  src = builtins.filterSource (path: type: type != "unknown") ./.;

  buildInputs = [ cmake boost gmp mpfr libedit python texinfo gnused ];

  enableParallelBuilding = true;

  # Skip byte-compiling of emacs-lisp files because this is currently
  # broken in ledger...
  postInstall = ''
    mkdir -p $out/share/emacs/site-lisp/
    cp -v "$src/lisp/"*.el $out/share/emacs/site-lisp/
  '' + stdenv.lib.optionalString stdenv.isDarwin ''
    for i in date_time filesystem system iostreams regex unit_test_framework; do
      boostlib=libboost_''$i.dylib
      install_name_tool -change ''$boostlib ${boost}/lib/''$boostlib $out/bin/ledger
    done
  '';

  meta = {
    homepage = "http://ledger-cli.org/";
    description = "A double-entry accounting system with a command-line reporting interface";
    license = "BSD";

    longDescription = ''
      Ledger is a powerful, double-entry accounting system that is accessed
      from the UNIX command-line. This may put off some users, as there is
      no flashy UI, but for those who want unparalleled reporting access to
      their data, there really is no alternative.
    '';

    platforms = stdenv.lib.platforms.all;
    maintainers = with stdenv.lib.maintainers; [ simons the-kenny jwiegley ];
  };
}
