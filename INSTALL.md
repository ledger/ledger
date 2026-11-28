# INSTALL

To build this code after doing a Git clone, run:

    $ ./acprep dependencies
    $ ./acprep update

If anything goes wrong, see [COMMON CONFIGURE/BUILD PROBLEMS](#common-configure--build-problems)

If you try to configure and build without running acprep first, you are
almost certainly going to run into problems.  In future, you can run
`acprep update` again and again, and it will keep you updated to the
very latest version.

Now install it:

    $ sudo make install


## COMMON CONFIGURE / BUILD PROBLEMS

To build and install Ledger requires several dependencies on various
platforms.  You can install these dependencies very simply for most of them
using:

    $ ./acprep dependencies

The first order of business if acprep update doesn't work is to find out where
things went wrong.  So follow these steps to produce a bug report I can track
down easily:

    $ ./acprep --debug update          # shows what acprep was thinking
    $ $EDITOR CMakeCache.txt           # shows what cmake was thinking

With the contents of `CMakeCache.txt` and `CMakeFiles/CMakeConfigureLog.yaml`
(CMake 3.26 and later; earlier releases write `CMakeFiles/CMakeOutput.log` and
`CMakeFiles/CMakeError.log`), and the output from acprep --debug update, it's
usually fairly obvious where things have gone astray.


## F.A.Q.


Q: `./acprep update` gives errors or `./acprep dependencies` fails

A: You're probably missing some dependency libraries.  If you tried
  `./acprep dependencies` already and that didn't solve the problem,
  then you may need to install dependencies by hand.  The packages for
  each platform are listed in the
  [Dependencies section of README.md](README.md#dependencies).

----------------------------------------------------------------------

Q: Configure fails saying it can't find boost_regex

A: Look in `CMakeFiles/CMakeConfigureLog.yaml` (`CMakeFiles/CMakeError.log`
  before CMake 3.26) in the build directory and search for "boost_regex", then
  scroll down a bit until you see the exact compile error.  Usually it's
  failing because your Boost is installed somewhere acprep is not expecting.
  It could also be failing because your Boost libraries have a custom
  "suffix" on them.

  Let's say your Boost was installed in ~/boost, and every library has the
  suffix `-xgcc42`.  This is what you would run:

    $ ./acprep --boost=$HOME/boost --boost-suffix=-xgcc42 update

----------------------------------------------------------------------

Q: Configure fails saying it can't find MPFR

A: You need MPFR version 4.0.2 or higher.  The relevant Debian and Ubuntu
  package is `libmpfr-dev`.  See also the question above about what to do
  if `./acprep update` gives errors or `./acprep dependencies` fails.

----------------------------------------------------------------------

Q: I'm seeing a segfault deep inside the boost_regex code!

A: Actually, the real segfault is in libstdc++'s facet code.  It's being
  caused by using a debug Boost with a non-debug build of Ledger, or
  vice-versa.

----------------------------------------------------------------------

Q: Something else fails, or Ledger crashes on startup

A: This, I am most interested in hearing about.  Please
  [file a bug report](https://github.com/ledger/ledger/issues/new) at the
  [Ledger Issue Tracker](https://github.com/ledger/ledger/issues).  The more
  details you can provide, the better.  Also, if Ledger is crashing, try
  running it under a debugger, e.g. gdb or lldb, like so:

    $ gdb ledger
    (gdb) run <ARGS TO LEDGER>
    ... runs till crash ...
    (gdb) bt

  Put that backtrace output, and the output from `ledger --version`
  in the bug report.

----------------------------------------------------------------------

Q: Whenever I try to use the Python support, I get a segfault

A: Make sure that the boost_python library you linked against is using the
  exact same Python as the Ledger executable.  In particular I see this
  bug on macOS systems where boost_python is linked against the default
  Python, while Ledger is linked against the version provided by MacPorts.
  Or vice versa.

  Solution: Use one or the other.  Configure Ledger for the Python that
  boost_python was built against with `./acprep --python --python-home DIR`,
  where DIR is that Python's installation prefix (`--python-version X.Y` only
  sets the oldest Python that CMake will accept).  You'll then need to delete
  the Ledger binary and run `make` to relink it.

----------------------------------------------------------------------

Q: When I run `make check`, the Python unit tests always crash

A: This can happen for the same reason as above.  It can also happen if you
  have ICU support enabled.  This is a bug I'm still trying to track down.

----------------------------------------------------------------------

Q: My distribution has versions of Boost and/or CMake that are too old for
  Ledger.  How do I build my own Boost and/or CMake binaries that will
  work properly with Ledger?  Thereafter, how do I configure Ledger
  properly to use those newly built versions of Boost and/or CMake?

A: Here's commands that one user used to make this work, for Boost 1.72.0
  on Debian GNU/Linux 11 (aka Debian bullseye).  It's likely to work ok
  for other versions of Boost as well.  [YMMV] on other distributions and/or
  other Debian versions, though.

  - Preparing and building Boost

        $ export BOOST_VERSION=1.72.0
        $ cd /somewhere/you/want/to/build/boost
        $ wget -N https://archives.boost.io/release/$BOOST_VERSION/source/boost_${BOOST_VERSION//./_}.tar.gz
        $ tar xvf boost_${BOOST_VERSION//./_}.tar.gz
        $ cd boost_${BOOST_VERSION//./_}
        $ ./bootstrap.sh
        $ ./b2 --build-type=complete --layout=tagged --prefix=/where/you/want/boost/installed
        $ ./b2 --build-type=complete --layout=tagged --prefix=/where/you/want/boost/installed install

  - Preparing and building CMake

        $ export CMAKE_VERSION=3.16.2
        $ cd /somewhere/you/want/to/build/cmake
        $ wget -N https://cmake.org/files/v${CMAKE_VERSION:0:-2}/cmake-${CMAKE_VERSION}.tar.gz
        $ tar xvf cmake-${CMAKE_VERSION}.tar.gz
        $ cd cmake-${CMAKE_VERSION}
        $ ./configure --prefix=/where/you/want/cmake/installed/
        $ make
        $ make install

  - Building Ledger using the CMake and/or Boost as installed above

        $ cd /path/to/ledger/sources
        $ env PATH=/where/you/want/cmake/installed/bin:$PATH PREFIX=/where/you/want/ledger/installed $SHELL
        $ ./acprep --prefix=$PREFIX --boost=/where/you/want/boost/installed --debug config
        $ ./acprep --prefix=$PREFIX --debug make
        $ ./acprep --prefix=$PREFIX --debug make install

  To build the Python bindings as well, add `--python` to the `config` step.
  They need Python 3.10 or later (Debian 12 and later; bullseye's python3 is
  3.9) and a boost_python built against that Python.

[YMMV]: https://www.acronymfinder.com/Your-Mileage-May-Vary-(YMMV).html
