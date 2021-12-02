{
  description = "A double-entry accounting system with a command-line reporting interface";

  outputs = { self, nixpkgs }: let
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
  in {

    packages = forAllSystems (system: let
        pkgs = nixpkgsFor.${system};
      in {
      ledger = pkgs.stdenv.mkDerivation {
        pname = "ledger";
        version = "3.2.1-${self.shortRev or "dirty"}";

        src = self;

        nativeBuildInputs = with pkgs; [ cmake ];
        buildInputs = with pkgs; [ boost gmp mpfr libedit python3 texinfo gnused gpgme ];

        enableParallelBuilding = true;

        cmakeFlags = [ "-DCMAKE_INSTALL_LIBDIR=lib" "-DUSE_GPGME=1" ];

        checkPhase = ''
          export LD_LIBRARY_PATH=$PWD
          ctest -j$NIX_BUILD_CORES
        '';

        doCheck = true;

        meta = {
          homepage = "http://ledger-cli.org/";
          description = "A double-entry accounting system with a command-line reporting interface";
          license = pkgs.lib.licenses.bsd3;

          longDescription = ''
            Ledger is a powerful, double-entry accounting system that is accessed
            from the UNIX command-line. This may put off some users, as there is
            no flashy UI, but for those who want unparalleled reporting access to
            their data, there really is no alternative.
          '';

          platforms = pkgs.lib.platforms.all;
          maintainers = with pkgs.lib.maintainers; [ jwiegley ];
        };
      };
    });

    defaultPackage = forAllSystems (system: self.packages.${system}.ledger);

  };
}
