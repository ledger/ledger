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
          gpgme gpgmepp
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
        ] ++ lib.optionals usePython [
          # Install the Python module into our own 'py' output rather than the
          # read-only Python store path.
          "-DLEDGER_PYTHON_INSTALL_DIR=${placeholder "py"}/${python3.sitePackages}"
          # Install Python examples (demo.py) into the 'py' output as well.
          "-DLEDGER_PYTHON_EXAMPLES_INSTALL_DIR=${placeholder "py"}/share"
        ];

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
        # Build Boost with Python support, matching exactly what the ledger
        # package uses.  A local binding lets us reference the same derivation
        # in both inputsFrom (via the ledger package) and in the explicit
        # Boost_DIR cmake flag below.
        boostWithPython = pkgs.boost.override {
          enablePython = true;
          python = pkgs.python3;
        };
      in with pkgs; {
      default = mkShellNoCC {
        name = "ledger-dev";

        inputsFrom = [ self.packages.${system}.ledger ];

        packages = [
          llvmPackages_18.clang-tools # Provides clang-format (pinned to match CI)
          llvmPackages_18.clang       # Clang compiler for profiling builds
          llvmPackages_18.llvm        # Provides llvm-profdata, llvm-cov
          hyperfine                   # Statistical benchmarking
          cppcheck
          doxygen                     # API documentation generator
          graphviz                    # Graph visualization for Doxygen call graphs
          (texliveSmall.withPackages (ps: [ ps.texinfo ])) # TeX for texi2pdf (PDF manual)
          lcov                        # Frontend for gcov (coverage analysis tool)
        ] ++ lib.optionals (system == "x86_64-linux" || system == "aarch64-linux") [
          gcc                         # GNU compiler suite, includes gcov for Linux
        ];

        # Point CMake at the ICU runtime package so FindICU can locate
        # libicuuc.  Nix splits headers (-dev) and libraries into separate
        # store paths; without this hint CMake finds the version from the
        # -dev headers but cannot locate the shared libraries.
        ICU_ROOT = "${pkgs.icu.out}";

        # Default CMake flags for the dev shell.  mkShellNoCC exports
        # this as $cmakeFlags so developers can use:
        #   cmake -B build $cmakeFlags
        # All optional features are enabled since every dependency is
        # available via inputsFrom.  Sanitizers, coverage, and profiling
        # are left off as they require dedicated build configurations.
        cmakeFlags = [
          "-DCMAKE_BUILD_TYPE=Debug"
          "-DUSE_PYTHON=ON"
          "-DUSE_DOXYGEN=ON"
          "-DUSE_GPGME=ON"
          "-DBUILD_DOCS=ON"
          # Explicitly pin Boost_DIR to the Nix-provided Boost with Python.
          # Without this, a stale CMakeCache.txt from a previous configure (e.g.
          # one that ran when a different Boost version was current) can leave a
          # cached Boost_DIR pointing to the wrong version.  That causes the
          # internal find_package(boost_python) inside BoostConfig.cmake to fail
          # with a version mismatch even though the correct Boost is available.
          "-DBoost_DIR=${boostWithPython.dev}/lib/cmake/Boost-${boost.version}"
          # Pin CMake to the Nix-provided Python so it doesn't pick up a
          # Homebrew Python (e.g. 3.14) that mismatches the Boost.Python build.
          "-DPython_EXECUTABLE=${pkgs.lib.getBin python3}/bin/python3"
        ];

        shellHook = ''
          echo "Ledger development environment"
          echo "  cmake -B build \$cmakeFlags   # configure with all features"
          echo "clang-format version: $(clang-format --version)"
          echo "Coverage tools available:"
          ${if system == "x86_64-linux" || system == "aarch64-linux" then ''
          echo "  - gcov: $(gcov --version | head -1)"
          '' else ''
          echo "  - gcov: Not available on $(uname)"
          ''}
          echo "  - lcov: $(lcov --version | head -1)"
          echo "  - llvm-cov: $(llvm-cov --version | grep -o 'version.*')"
        '';
      };
    });

  };
}
