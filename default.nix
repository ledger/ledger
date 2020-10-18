{ packages ? "llvmPackages_9"

, rev    ? "1fe82110febdf005d97b2927610ee854a38a8f26"
, sha256 ? "08x6saa7iljyq2m0j6p9phy0v17r3p8l7vklv7y7gvhdc7a85ppi"

, pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
    overlays = [
      (self: super: {})
    ];
  }
}:

let
  version = "3.2.1";
  rev = "20200518";
in

pkgs.stdenv.mkDerivation {
  name = "ledger-${version}-${rev}";

  src = ./.;

  nativeBuildInputs = with pkgs; [ cmake ];
  buildInputs = with pkgs; [ boost gmp mpfr libedit python3 texinfo gnused gpgme ];

  enableParallelBuilding = true;

  cmakeFlags = [ "-DCMAKE_INSTALL_LIBDIR=lib" "-DUSE_GPGME=1" ];

  buildPhase = "make -j$NIX_BUILD_CORES";
  checkPhase = ''
    export LD_LIBRARY_PATH=$PWD
    ctest -j$NIX_BUILD_CORES
  '';

  doCheck = true;

  meta = {
    homepage = "http://ledger-cli.org/";
    description = "A double-entry accounting system with a command-line reporting interface";
    license = pkgs.stdenv.lib.licenses.bsd3;

    longDescription = ''
      Ledger is a powerful, double-entry accounting system that is accessed
      from the UNIX command-line. This may put off some users, as there is
      no flashy UI, but for those who want unparalleled reporting access to
      their data, there really is no alternative.
    '';

    platforms = pkgs.stdenv.lib.platforms.all;
    maintainers = with pkgs.stdenv.lib.maintainers; [ jwiegley ];
  };
}
