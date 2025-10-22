{
  description = "A double-entry accounting system with a command-line reporting interface";

  nixConfig.bash-prompt = "ledger$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url  = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      usePython = true;
      gpgmeSupport = true;
      useLibedit = true;
      useReadline = false;
      overlays = [ (import rust-overlay) ];
      pkgs = import nixpkgs {
        inherit system overlays;
      };
    in with pkgs; rec {
      packages.rledger = pkgs.rustPlatform.buildRustPackage {
        pname = "rledger";
        version = "3.5.0-${self.shortRev or "dirty"}";
        src = self;
        cargoHash = "sha256-YwSF08stNaZJ44vucXtjFe2UuJ8XxYD4roXOGbYVD80=";
      };

      packages.default = stdenv.mkDerivation {
        pname = "ledger";
        version = "3.3.2-${self.shortRev or "dirty"}";

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

      devShells.default = mkShell {
        buildInputs = packages.default.buildInputs ++ [
          openssl
          pkg-config
          eza
          fd
          rust-bin.beta.latest.default
          # darwin.apple_sdk.frameworks.Security
        ];

        shellHook = ''
          alias ls=eza
          alias find=fd
        '';
      };
    });
}
