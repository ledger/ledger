{ stdenv, fetchgit, cmake, ninja, boost, gmp, mpfr, libedit, python
, texinfo, doxygen, gnused, lcov }:

let
  rev = "20160111";
in

stdenv.mkDerivation {
  name = "ledger-3.1.1.${rev}";
  src = builtins.filterSource (path: type: type != "unknown") ./.;

  buildInputs = [ cmake ninja doxygen lcov boost gmp mpfr libedit
                  python texinfo gnused ];

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
    maintainers = with stdenv.lib.maintainers; [ simons the-kenny jwiegley ];
  };
}
