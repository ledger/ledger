# Ledger: Command-Line Accounting

Ledger is a powerful, double-entry accounting system that is accessed from the
UNIX command-line.  This may put off some users, since there is no flashy UI,
but for those who want unparalleled reporting access to their data there are
few alternatives.

## For the Impatient

I know, you just want to build and play.  If you have all the dependencies
installed (see below), then simply do this:

    git clone git://github.com/ledger/ledger.git
    cd ledger && ./acprep update  # Update to the latest, configure, make

Now try your first ledger command:

    ./ledger -f test/input/sample.dat reg

## To the Rest

If you're reading this file, you have in your hands the Bleeding Edge.  This
may very well *not* be what you want, since it's not guaranteed to be in a
functionally complete state.  It's under active development, and may change in
any way at any time.

What you may prefer is the current stable release, or the current beta branch.
The **BETA** is what I prefer people use, since I still have a chance to fix
major bugs that you find.  Just e-mail me, or post to the mailing list,
they'll become a part of my work list.

<table>
<tr><td><strong>RELEASE</strong></td><td><tt>git checkout v2.6.3</tt></td></tr>
<tr><td><strong>CURRENT</strong></td><td><tt>git checkout maint</tt></td></tr>
<tr><td><strong>BETA</strong></td><td><tt>git checkout -b master origin/master</tt></td></tr>
<tr><td><strong>ALPHA</strong></td><td><tt>git checkout -b next origin/next</tt></td></tr>
</table>

There are also several topic branches which contain experimental features,
though none of these are guaranteed to compile.  Best to chat with me on
[IRC](irc://irc.freenode.net/ledger) or via the
[mailing list](http://groups.google.com/group/ledger-cli) before going too
much further with those.

## Dependencies

If you wish to proceed in this venture, you'll need a few dependencies.  The
easiest way to get them for your platform is to run:

    ./acprep dependencies

If that doesn't completely work, here are the dependencies for building the
current `master` branch:

<table>
<tr><td>Boost</td><td>1.35</td><td></td></tr>
<tr><td>GMP</td><td>4.2.2</td><td></td></tr>
<tr><td>MPFR</td><td>2.4.0</td><td></td></tr>
<tr><td>gettext</td><td>0.17</td><td><em>optional</em></td></tr>
<tr><td>libedit</td><td>20090111-3.0</td><td><em>optional</em></td></tr>
<tr><td>Python</td><td>2.4</td><td><em>optional</em></td></tr>
<tr><td>doxygen</td><td>1.5.7.1</td><td><em>optional</em>, for <tt>make docs</tt></td></tr>
<tr><td>graphviz</td><td>2.20.3</td><td><em>optional</em>, for <tt>make docs</tt></td></tr>
<tr><td>texinfo</td><td>4.13</td><td><em>optional</em>, for <tt>make docs</tt></td></tr>
<tr><td>lcov</td><td>1.6</td><td><em>optional</em>, for <tt>make report</tt>, used with <tt>/./acprep gcov</tt></td></tr>
<tr><td>sloccount</td><td>2.26</td><td><em>optional</em>, for <tt>make sloc</tt></td></tr>
</table>

And for building the current `maint` branch:

<table>
<tr><td>GMP</td><td>4.2.2</td><td> </td></tr>
<tr><td>pcre</td><td>7.7</td><td> </td></tr>
<tr><td>libofx</td><td>0.8.3</td><td><em>optional</em></td></tr>
<tr><td>expat</td><td>2.0.1</td><td><em>optional</em></td></tr>
<tr><td>libxml2</td><td>2.7.2</td><td><em>optional</em></td></tr>
</table>

### MacPorts

If you build stuff using MacPorts on OS X, as I do, here is what you would
run:

    sudo port install -f cmake python26
        libiconv +universal zlib +universal gmp +universal
        mpfr +universal ncurses +universal ncursesw +universal
        gettext +universal libedit +universal boost-jam
        boost +st+python26+icu texlive doxygen graphviz
        texinfo lcov sloccount

### Ubuntu

If you're going to build on Ubuntu, `sudo apt-get install ...` the
following packages (current as of Ubuntu 12.04):

    sudo apt-get install build-essential cmake zlib1g-dev libbz2-dev
         python-dev gettext libgmp3-dev libmpfr-dev libboost-dev
         libboost-regex-dev libboost-date-time-dev
         libboost-filesystem-dev libboost-python-dev texinfo lcov
         sloccount libboost-iostreams-dev libboost-test-dev

Or, for Ubuntu Karmic:

    sudo apt-get install build-essential cmake texinfo python-dev zlib1g-dev
         libbz2-dev libgmp3-dev bjam gettext cvs libboost1.40-dev
         libboost-regex1.40-dev libboost-date-time1.40-dev
         libboost-filesystem1.40-dev libmpfr-dev

### Debian

Debian squeeze (6.0): the version of boost in squeeze is too old
for ledger and unfortunately no backport is available at the moment.

Debian wheezy (7.0) contains all components needed to build ledger.
You can install all required build dependencies using the following
command:

    sudo apt-get install build-essential cmake autopoint texinfo python-dev
         zlib1g-dev libbz2-dev libgmp3-dev gettext libmpfr-dev
         libboost-date-time1.49-dev libboost-filesystem1.49-dev
         libboost-graph1.49-dev libboost-iostreams1.49-dev
         libboost-python1.49-dev libboost-regex1.49-dev libboost-test1.49-dev

## Building

The next step is preparing your environment for building.  While you can use
`cmake .` and make, I've prepared a script that does a lot more of the
footwork for you:

    ./acprep update
    # or, if you want to use the Boost libraries with suffix -mt, install in
    # $HOME/local and build with 2 processes in parallel
    ./acprep update --boost-suffix=-mt -- --prefix=$HOME/local -j2

Please read the contents of `config.log` if the configure step fails.  Also,
see the `help` subcommand to `acprep`, which explains some of its many
options.  It's pretty much the only command I run for configuring, building
and testing Ledger.

You can run `make check` to confirm the result, and `make install` to install.

## Resources

Now that you're up and running, here are a few resources to keep in mind:

 - [Home page](http://ledger-cli.org)
 - [IRC channel](irc://irc.freenode.net/ledger)
 - [Mailing List / Forum](http://groups.google.com/group/ledger-cli)
 - [GitHub project page](http://github.com/ledger/ledger)
 - [Ohloh code analysis](http://www.ohloh.net/projects/ledger)

If you have ideas you'd like to share, the best way is either to e-mail me a
patch (I prefer attachments over pasted text), or to get an account on GitHub.
Once you do, fork the [Ledger project](http://github.com/ledger/ledger),
hack as much as you like, then send me a pull request via GitHub.
