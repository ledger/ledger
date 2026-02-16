{
  description = "A double-entry accounting system with a command-line reporting interface";

  nixConfig.bash-prompt = "ledger$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }: let
    usePython = true;
    gpgmeSupport = true;
    useLibedit = true;
    useReadline = false;
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
  in {

    packages = forAllSystems (system: let
        pkgs = nixpkgsFor.${system};
      in with pkgs; {
      ledger = stdenv.mkDerivation {
        pname = "ledger";
        version = "3.4.1-${self.shortRev or "dirty"}";

        src = self;

        outputs = [ "out" "dev" ] ++ lib.optionals usePython [ "py" ];

        buildInputs = [
          gmp mpfr gnused icu
        ] ++ lib.optionals useLibedit [
          libedit
        ] ++ lib.optionals useReadline [
          readline
        ] ++ lib.optionals gpgmeSupport [
          gpgme
        ] ++ (if usePython
              then [ python3 (boost.override { enablePython = true; python = python3; }) ]
              else [ boost ]);

        nativeBuildInputs = [
          cmake texinfo tzdata
        ] ++ lib.optionals useLibedit [
          libedit.dev
        ] ++ lib.optionals useReadline [
          readline.dev
        ];

        enableParallelBuilding = true;

        cmakeFlags = [
          "-DCMAKE_INSTALL_LIBDIR=lib"
          "-DBUILD_DOCS:BOOL=ON"
          "-DUSE_PYTHON:BOOL=${if usePython then "ON" else "OFF"}"
          "-DUSE_GPGME:BOOL=${if gpgmeSupport then "ON" else "OFF"}"
        ];

        # by default, it will query the python interpreter for its sitepackages location
        # however, that would write to a different nixstore path, pass our own sitePackages location
        prePatch = lib.optionalString usePython ''
          substituteInPlace src/CMakeLists.txt \
            --replace 'DESTINATION ''${Python_SITEARCH}' 'DESTINATION "${placeholder "py"}/${python3.sitePackages}"'
        '';

        installTargets = [ "doc" "install" ];

        checkPhase = ''
          runHook preCheck
          env LD_LIBRARY_PATH=$PWD \
            DYLD_LIBRARY_PATH=$PWD \
            ctest -j$NIX_BUILD_CORES
          runHook postCheck
        '';

        doCheck = true;

        meta = with lib; {
          description = "A double-entry accounting system with a command-line reporting interface";
          homepage = "https://ledger-cli.org/";
          changelog = "https://github.com/ledger/ledger/raw/v${version}/NEWS.md";
          license = lib.licenses.bsd3;
          longDescription = ''
            Ledger is a powerful, double-entry accounting system that is accessed
            from the UNIX command-line. This may put off some users, as there is
            no flashy UI, but for those who want unparalleled reporting access to
            their data, there really is no alternative.
          '';
          platforms = lib.platforms.all;
          maintainers = with maintainers; [ jwiegley ];
        };
      };
    });

    defaultPackage = forAllSystems (system: self.packages.${system}.ledger);

    devShells = forAllSystems (system: let
        pkgs = nixpkgsFor.${system};
      in with pkgs; {
      default = mkShellNoCC {
        name = "ledger-dev";

        inputsFrom = [ self.packages.${system}.ledger ];

        packages = [
          llvmPackages_18.clang-tools # Provides clang-format (pinned to match CI)
          llvmPackages_18.clang       # Clang compiler for profiling builds
          llvmPackages_18.llvm        # Provides llvm-profdata, llvm-cov
          hyperfine                   # Statistical benchmarking
          lcov                        # Frontend for gcov (coverage analysis tool)
        ] ++ lib.optionals (system == "x86_64-linux" || system == "aarch64-linux") [
          gcc                         # GNU compiler suite, includes gcov for Linux
        ];

        shellHook = ''
          echo "Ledger development environment"
          echo "clang-format version: $(clang-format --version)"
          echo "Coverage tools available:"
          ${if system == "x86_64-linux" || system == "aarch64-linux" then ''
          echo "  - gcov: $(gcov --version | head -1)"
          '' else ''
          echo "  - gcov: Not available on $(uname)"
          ''}
          echo "  - lcov: $(lcov --version | head -1)"
          echo "  - llvm-cov: $(llvm-cov --version | head -1)"
        '';
      };
    });

  };
}
