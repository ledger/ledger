Tips for contributors
---------------------

* Please **make pull requests against `main`**.
* Please add a **test case** under `test/regress` when possible.
* If you're making **changes to files for which the CI build is not
  relevant**, please **add `[ci skip]` to the end of the commit message**.
* Report bugs using [GitHub Issues].

GLOSSARY
----

Developing the Ledger software uses a number different tools, not all of
which will be familiar to all developers.

**[Boost]**: a standard set of C++ libraries.  Most
Boost libraries consist of inline functions and templates in header files.

**[Boost.Python]**:  C++ library which enables seamless interoperability
between C++ and the Python programming language.

**[CMake]**: A cross platform system for building from source code.  It uses
the `CMakeLists.txt` files.

**[Doxygen]**: generates programming documentation from
source code files.  Primarily used on C++ sources, but works on all.
Configured in `doc/CMakeLists.txt` through `doxygen_add_docs`; enable it with
`-DUSE_DOXYGEN=ON` (or `./acprep --doxygen`) and build the `doc.doxygen` target.

**[GCC]**: Gnu Compiler Collection, which includes the
*gcc* compiler and *gcov* coverage/profiler tool.

**[clang]**: C language family frontend for LLVM, which
includes the *clang* compiler.

**[GMP]**: Gnu Multiple Precision Arithmetic Library
provides arbitrary precision math.

**[MPFR]**: Gnu Multiple Precision Floating-point Library
with correct rounding.

**[Markdown]**: A typesetter
format that produces *html* files from *.md* files.  Note that GitHub
automatically renders *.md* files.

**[SHA1]**: a marginally secure cryptographic hash function.  Ledger uses
Boost's implementation (`sha1sum()` in `src/utils.h`) to hash payees and
account names under `--anon` and, in `convert`, to compute the
duplicate-detection reference of a CSV line that has no UUID.  `--hashes`
uses SHA-512 (`src/sha512.cc`) instead.

**[Texinfo]**: Gnu documentation
typesetter that produces *html* and *pdf* files from the `doc/\*.texi` files.

**[GitHub Actions]**: a hosted continuous integration
  service that builds and runs tests each commit posted to GitHub.  Each
  build creates a log, updates a [small badge] at
  the top left of the main project's
  [README.md], and
  emails the author of the commit if any tests fail.

**[utfcpp]**: a library for handling utf-8 in a variety of C++ versions.


Orientation
---

The source tree can be confusing to a new developer.  Here is a selective
orientation:

**./acprep**: a custom thousand-line script to install dependencies, grab
  updates, and build.  It also creates `\*.cmake`,
  `./CmakeFiles/` and other CMake temporary files.  Use `./acprep --help`
  for more information.

**./README.md**: user readme file in markdown format, also used as the project
  description on GitHub.

**./benchmark/**: the CI performance gate (`run_benchmark.sh` times two
  ledger binaries and `compare_benchmarks.py` compares the results) and
  `profile.sh`, a Clang profiling and PGO helper.

**./cmake/**: CMake find modules for the bundled utfcpp and immer, plus
  packaging, coverage, lint and Python-module helpers.

**./contrib/**: contributed scripts of random quality and completion.  They
  usually require editing to run.

**./doc/**: documentation, licenses, and
  tools for generating documents such as the *pdf* manual.

**./lib/**: bundled copies of utfcpp and immer, used when no system copy is
  found.

**./python/**:  samples using the Python ledger module.

**./scripts/**:  `sanitizers.sh`, which builds Ledger with AddressSanitizer
  and UndefinedBehaviorSanitizer and runs the test suite.

**./src/**:  the C++ header and source files in a flat directory.

**./test/**:  a testing harness with subdirectories full of tests.  See
  `test/snapshot.md` for the behavioral snapshot tool.

**./tools/**:  an accretion of tools, mostly small scripts, to aid development


Building
---

If you are going to be working on Ledger, you'll want a debug build, which is
the default flavor of `acprep`.  Pre-compiled headers for `system.hh` are on
by default for every build type and compiler (the `PRECOMPILE_SYSTEM_HH` CMake
option; configure with `-DPRECOMPILE_SYSTEM_HH=OFF` to turn them off).  To
choose your compiler, pass `--compiler`:

    mkdir build
    ./acprep --compiler=clang++
    cd build/ledger/debug
    make

This will set up a debug build using clang++, and then start a build.

For even quicker rebuilds, try the Ninja build tool, which is very fast at
determining what to rebuild, and automatically takes advantage of multiple
cores:

    mkdir build
    ./acprep --compiler=clang++ --ninja
    cd build/ledger/debug
    ninja

[Boost]: https://boost.org
[Boost.Python]: https://www.boost.org/libs/python/
[GitHub Issues]: https://github.com/ledger/ledger/issues
[GMP]: https://gmplib.org/
[MPFR]: https://www.mpfr.org/
[CMake]: https://www.cmake.org
[Doxygen]: https://doxygen.org
[Markdown]: https://daringfireball.net/projects/markdown/
[SHA1]: https://en.wikipedia.org/wiki/SHA-1
[Texinfo]: https://www.gnu.org/software/texinfo/
[GitHub Actions]: https://github.com/features/actions
[GCC]: https://gcc.gnu.org
[utfcpp]: https://utfcpp.sourceforge.net
[small badge]: https://github.com/ledger/ledger/actions/workflows/cmake.yml/badge.svg
[README.md]: https://github.com/ledger/ledger/blob/main/README.md
[clang]: https://clang.llvm.org
