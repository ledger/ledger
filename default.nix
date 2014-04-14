{ stdenv, fetchgit, cmake, boost, gmp, mpfr, libedit, python, texinfo }:

stdenv.mkDerivation {
  name = "ledger-3.0.1";
  version = "3.0.1";

  src = ./.;

  buildInputs = [ cmake boost gmp mpfr libedit python texinfo ];

  # Tests on Darwin are failing
  doCheck = !stdenv.isDarwin;
  enableParallelBuilding = true;

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
    maintainers = with stdenv.lib.maintainers; [ simons the-kenny ];
  };
}
