[![Join the chat at https://gitter.im/use-package/Lobby](https://badges.gitter.im/use-package/Lobby.svg)](https://gitter.im/use-package/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
![Build Status master](https://github.com/ledger/ledger/actions/workflows/cmake.yml/badge.svg)
[![Status](https://img.shields.io/badge/status-active-brightgreen.svg?style=flat)](https://github.com/ledger/ledger/pulse/monthly)
[![License](https://img.shields.io/badge/license-BSD-blue.svg?style=flat)](https://opensource.org/licenses/BSD-3-Clause)
[![GitHub release](https://img.shields.io/github/release/ledger/ledger.svg?style=flat)](https://github.com/ledger/ledger/releases)

# Ledger: Command-Line Accounting

Ledger is a powerful, double-entry accounting system that is accessed from the
UNIX command-line.  This may put off some users, since there is no flashy UI,
but for those who want unparalleled reporting access to their data there are
few alternatives.

Ledger uses text files for input.  It reads the files and generates reports;
there is no other database or stored state.  To use Ledger, you create a
file of your account names and transactions, run from the command line with
some options to specify input and requested reports, and get output.
The output is generally plain text, though you could generate a graph or
html instead.  Ledger is simple in concept, surprisingly rich in ability,
and easy to use.


## For the Impatient

I know, you just want to build and play.  If you have all the [dependencies](#dependencies)
installed, then simply do this:

    $ git clone git@github.com:ledger/ledger.git
    $ cd ledger && ./acprep update  # Update to the latest, configure, make

Now try your first ledger command:

    $ ./ledger -f test/input/sample.dat reg

For help on keeping your journal have a look at the
[documentation] and the [wiki][] (Also see the “Resources” section at the
end of this file). An Emacs mode for Ledger files can be found in the
[ledger/ledger-mode repository] and a vim plugin is located in the
[ledger/vim-ledger repository].

## Docker version

If you have Docker installed on your computer or server, you can use a [Docker version](https://hub.docker.com/r/dcycle/ledger/) of this software, without installing any further dependencies:

    $ docker run --rm -v "$PWD"/test/input:/data dcycle/ledger:1 -f /data/sample.dat reg

## Dependencies

If you wish to proceed in this venture, you'll need a few dependencies.  The
easiest way to get them for your platform is to run this handy Python
script:

    $ ./acprep dependencies

Note that some features, e.g. `--import` require building Ledger with
Python support.

If that doesn't completely work, here are the dependencies for building the
current `master` branch:

Dependency  | Version (or greater)
------------|---------------------
[CMake]     | 3.16.2
[Boost]     | 1.72
[Gmp]       | 6.1.2
[Mpfr]      | 4.0.2
[utfcpp]    | 3.2.3
[gettext]   | 0.17 _optional_
[libedit]   | 20090111-3.0 _optional_
[Python]    | 3.9 _optional_
[Gpgmepp]   | 1.13.1 _optional_
[doxygen]   | 1.5.7.1 _optional_, for `make docs`
[graphviz]  | 2.20.3 _optional_, for `make docs`
[texinfo]   | 4.13 _optional_, for `make docs`
[lcov]      | 1.6 _optional_, for `make report`, used with `./acprep gcov`
[sloccount] | 2.26 _optional_, for `make sloc`

### macOS

You can use [Homebrew] or [MacPorts] to install build dependencies for Ledger
easily on macOS.

#### 1. Homebrew

If you use Homebrew, to install the dependencies you would run:

    $ brew install cmake boost boost-python3 gmp mpfr

#### 2. MacPorts

If you build stuff using MacPorts on macOS, as I do, here is what you would
run:

    $ sudo port install -f cmake python37 \
         libiconv zlib gmp \
         mpfr ncurses ncursesw \
         gettext libedit boost-jam \
         boost +st+python37+icu texlive doxygen graphviz \
         texinfo lcov sloccount

### Conda

The dependencies for building Ledger are available from [conda-forge] on certain
platforms (for example, `linux-64`), which can be used with [Conda] or [mamba].

With Conda you could run:

    $ conda install -c conda-forge python=3 cmake boost gmp mpfr \
         gettext libedit texinfo doxygen graphviz

### Ubuntu

If you're going to build on Ubuntu, `sudo apt-get install ...` the
following packages (current as of Ubuntu 18.04):

    $ sudo apt-get install build-essential cmake doxygen \
         libboost-system-dev libboost-dev python3-dev gettext git \
         libboost-date-time-dev libboost-filesystem-dev \
         libboost-iostreams-dev libboost-python-dev libboost-regex-dev \
         libboost-test-dev libedit-dev libgmp3-dev libmpfr-dev texinfo tzdata

### Debian

Debian 10 (bullseye), Debian 11 ("bullseye"), Debian testing and Debian
unstable (sid) contain all components needed to build ledger.  You can
install all required build dependencies using the following command:

    $ sudo apt-get install build-essential cmake autopoint texinfo python3-dev \
         zlib1g-dev libbz2-dev libgmp3-dev gettext libmpfr-dev \
         libboost-date-time-dev libboost-filesystem-dev \
         libboost-graph-dev libboost-iostreams-dev \
         libboost-python-dev libboost-regex-dev libboost-test-dev

### Fedora

You can install all required build dependencies under Fedora using the
following command (tested with Fedora 32):

    $ sudo dnf install boost-date-time boost-devel boost-filesystem \
        boost-iostreams boost-python3-devel boost-regex boost-system \
        boost-test cmake doxygen gettext git gmp-devel libedit-devel \
        mpfr-devel python3-devel texinfo tzdata

## Building

The next step is preparing your environment for building.  While you can use
`cmake .` and make, I've prepared a script that does a lot more of the
footwork for you:

    $ ./acprep update
    # or, if you want to use the Boost libraries with suffix -mt, install in
    # $HOME/local and build with 2 processes in parallel
    $ ./acprep update --boost-suffix=-mt --prefix=$HOME/local -j2

Please read the contents of `CMakeFiles/CMakeOutput.log` and
`CMakeFiles/CMakeError.log` if the configure step fails.  Also,
see the `help` subcommand to `acprep`, which explains some of its many
options.  It's pretty much the only command I run for configuring, building
and testing Ledger.

You can run `make check` to confirm the result, and `make install` to install.

## Resources

Now that you're up and running, here are a few resources to keep in mind:

 - [Homepage]
 - [Documentation]
 - [IRC channel][IRC]: #ledger channel on Libera Chat
 - [Mailing List / Forum][mailing list]
 - [GitHub project page][github]
 - [Code analysis][openhub]

If you have ideas you'd like to share, the best way is either to e-mail me a
patch (I prefer attachments over pasted text), or to get an account on GitHub.
Once you do, fork the [Ledger project][github],
hack as much as you like, then send me a pull request via GitHub.

[Homepage]: https://ledger-cli.org/
[documentation]: https://www.ledger-cli.org/docs.html
[mailing list]: https://list.ledger-cli.org/
[wiki]: https://wiki.ledger-cli.org/
[IRC]: irc://irc.libera.chat/ledger
[github]: https://github.com/ledger/ledger
[ledger/vim-ledger repository]: https://github.com/ledger/vim-ledger
[Homebrew]: https://brew.sh/
[MacPorts]: https://www.macports.org/
[CMake]: https://cmake.org
[Boost]: https://boost.org
[GMP]: https://gmplib.org/
[MPFR]: https://www.mpfr.org/
[utfcpp]: https://utfcpp.sourceforge.net
[gettext]: https://www.gnu.org/software/gettext/
[libedit]: https://thrysoee.dk/editline/
[Python]: https://python.org
[Gpgmepp]: https://www.gnupg.org/related_software/gpgme/
[doxygen]: https://www.doxygen.org/
[graphviz]: https://graphviz.org/
[texinfo]: https://www.gnu.org/software/texinfo/
[lcov]: https://ltp.sourceforge.net/coverage/lcov.php
[sloccount]: https://www.dwheeler.com/sloccount/
[pcre]: https://www.pcre.org/
[libofx]: https://libofx.sourceforge.net
[expat]: https://libexpat.github.io
<!--
xmlsoft url kept as http since its TLS certificate setup is incorrect and browser show
a "This Connection Is Not Private" message. [Last checked: 2023-03-13]
-->
[libxml2]: http://xmlsoft.org
[openhub]: https://www.openhub.net/p/ledger
[conda-forge]: https://conda-forge.org
[Conda]: https://conda.io
[mamba]: https://github.com/mamba-org/mamba
