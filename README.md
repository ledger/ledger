[![Join the chat at https://gitter.im/use-package/Lobby](https://badges.gitter.im/use-package/Lobby.svg)](https://gitter.im/use-package/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status master](https://img.shields.io/travis/ledger/ledger/master.svg?label=master&style=flat)](https://travis-ci.org/ledger/ledger)
[![Build Status next](https://img.shields.io/travis/ledger/ledger/next.svg?label=next&style=flat)](https://travis-ci.org/ledger/ledger)
[![Status](https://img.shields.io/badge/status-active-brightgreen.svg?style=flat)](https://github.com/ledger/ledger/pulse/monthly)
[![License](https://img.shields.io/badge/license-BSD-blue.svg?style=flat)](http://opensource.org/licenses/BSD-3-Clause)
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

    $ git clone git://github.com/ledger/ledger.git
    $ cd ledger && ./acprep update  # Update to the latest, configure, make

Now try your first ledger command:

    $ ./ledger -f test/input/sample.dat reg

For help on keeping your journal have a look at the
[documentation] and the [wiki][] (Also see the “Resources” section at the
end of this file). An Emacs mode for Ledger files can be found in the
[ledger/ledger-mode repository] and a vim plugin is located in the
[ledger/vim-ledger repository].

## To the Rest

If you're reading this file, you have in your hands the Bleeding Edge.  This
may very well *not* be what you want, since it's not guaranteed to be in a
functionally complete state.  It's under active development, and may change in
any way at any time.  What you may prefer is the **CURRENT** stable release, or
the **BETA** branch.

Branch | Command
-------|--------
**RELEASE**  | `git checkout -b stable v3.1` |
**CURRENT**  | `git checkout -b master origin/master` |
**BETA**     | `git checkout -b 3.1.1 release/3.1.1` |
**ALPHA**    | `git checkout -b next origin/next` |

There are also several topic branches which contain experimental features,
though none of these are guaranteed to compile.  Best to chat with me on
[IRC] or via the [mailing list] before going too much further with those.

## Dependencies

If you wish to proceed in this venture, you'll need a few dependencies.  The
easiest way to get them for your platform is to run this handy Python
script:

    $ ./acprep dependencies

If that doesn't completely work, here are the dependencies for building the
current `master` branch:

Dependency | Version (or greater)
-----------|---------------------
[Boost] | 1.49
[GMP] | 4.2.2
[MPFR] | 2.4.0
[utfcpp] | 2.3.4
[gettext] | 0.17 _optional_
[libedit] | 20090111-3.0 _optional_
[Python] | 2.4 _optional_
[doxygen] | 1.5.7.1 _optional_, for `make docs`
[graphviz] | 2.20.3 _optional_, for `make docs`
[texinfo] | 4.13 _optional_, for `make docs`
[lcov] | 1.6 _optional_, for `make report`, used with `/./acprep gcov`
[sloccount] | 2.26 _optional_, for `make sloc`

### macOS

You can use [Homebrew] or [MacPorts] to install Ledger easily on macOS.

#### 1. Homebrew

You can see the parameters you can pass while installing with brew by the command `brew options ledger`. To install ledger, simply type the following command:

    $ brew install ledger

If you to want to startup python, use the following command:

    $ ledger python


#### 2. MacPorts

If you build stuff using MacPorts on macOS, as I do, here is what you would
run:

    $ sudo port install -f cmake python26 \
         libiconv +universal zlib +universal gmp +universal \
         mpfr +universal ncurses +universal ncursesw +universal \
         gettext +universal libedit +universal boost-jam \
         boost +st+python26+icu texlive doxygen graphviz \
         texinfo lcov sloccount

### Ubuntu

If you're going to build on Ubuntu, `sudo apt-get install ...` the
following packages (current as of Ubuntu 18.04):

    $ sudo apt-get install build-essential cmake doxygen \
         libboost-system-dev libboost-dev python-dev gettext git \
         libboost-date-time-dev libboost-filesystem-dev \
         libboost-iostreams-dev libboost-python-dev libboost-regex-dev \
         libboost-test-dev libedit-dev libgmp3-dev libmpfr-dev texinfo

### Debian

Debian 9 (stretch), Debian 10 (buster), Debian testing and Debian unstable
(sid) contain all components needed to build ledger.  You can install all
required build dependencies using the following command:

    $ sudo apt-get install build-essential cmake autopoint texinfo python-dev \
         zlib1g-dev libbz2-dev libgmp3-dev gettext libmpfr-dev \
         libboost-date-time-dev libboost-filesystem-dev \
         libboost-graph-dev libboost-iostreams-dev \
         libboost-python-dev libboost-regex-dev libboost-test-dev

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
 - [IRC channel][IRC]
 - [Mailing List / Forum][mailing list]
 - [GitHub project page][github]
 - [Code analysis][openhub]

If you have ideas you'd like to share, the best way is either to e-mail me a
patch (I prefer attachments over pasted text), or to get an account on GitHub.
Once you do, fork the [Ledger project][github],
hack as much as you like, then send me a pull request via GitHub.

[Homepage]: http://ledger-cli.org/
[documentation]: http://www.ledger-cli.org/docs.html
[mailing list]: http://list.ledger-cli.org/
[wiki]: http://wiki.ledger-cli.org/
[IRC]: irc://irc.freenode.net/ledger
[github]: http://github.com/ledger/ledger
[ledger/vim-ledger repository]: https://github.com/ledger/vim-ledger
[Homebrew]: http://brew.sh/
[MacPorts]: https://www.macports.org/
[Boost]: http://boost.org
[GMP]: http://gmplib.org/
[MPFR]: http://www.mpfr.org/
[utfcpp]: http://utfcpp.sourceforge.net
[gettext]: https://www.gnu.org/software/gettext/
[libedit]: http://thrysoee.dk/editline/
[Python]: http://python.org
[doxygen]: http://www.doxygen.org/
[graphviz]: http://graphviz.org/
[texinfo]: http://www.gnu.org/software/texinfo/
[lcov]: http://ltp.sourceforge.net/coverage/lcov.php
[sloccount]: http://www.dwheeler.com/sloccount/
[pcre]: http://www.pcre.org/
[libofx]: http://libofx.sourceforge.net
[expat]: http://www.libexpat.org
[libxml2]: http://xmlsoft.org
[openhub]: https://www.openhub.net/p/ledger
