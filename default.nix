{ stdenv, fetchgit, cmake, boost, gmp, mpfr, libedit, python
, texinfo, gnused }:

let
  version = "3.1.2";
  rev = "20190105";
in

stdenv.mkDerivation {
  name = "ledger-${version}-${rev}";

  src = ./.;

  buildInputs = [ cmake boost gmp mpfr libedit python texinfo gnused ];

  enableParallelBuilding = true;

  cmakeFlags = [ "-DCMAKE_INSTALL_LIBDIR=lib" ];

  buildPhase = "make -j$NIX_BUILD_CORES";
  checkPhase = "ctest -j$NIX_BUILD_CORES";

  doCheck = true;

  meta = {
    homepage = "http://ledger-cli.org/";
    description = "A double-entry accounting system with a command-line reporting interface";
    license = stdenv.lib.licenses.bsd3;

    longDescription = ''
      Ledger is a powerful, double-entry accounting system that is accessed
      from the UNIX command-line. This may put off some users, as there is
      no flashy UI, but for those who want unparalleled reporting access to
      their data, there really is no alternative.
    '';

    platforms = stdenv.lib.platforms.all;
    maintainers = with stdenv.lib.maintainers; [ the-kenny jwiegley ];
  };
}
