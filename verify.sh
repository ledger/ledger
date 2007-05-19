#!/bin/bash

MY_CPPFLAGS="-I/usr/local/include -I/usr/local/include/boost -I/sw/include"
MY_LDFLAGS="-L/usr/local/lib -L/sw/lib"

# Setup the temporary directory where all these copies are ledger are
# going to be built.  Remove it if it's already there.

if [ -n "$1" -a -d "$1" ]; then
    TMPDIR="$1"
elif [ -d $HOME/tmp ]; then
    TMPDIR=$HOME/tmp
else
    TMPDIR=/tmp
fi

if [ -d $TMPDIR/ledger ]; then
    find $TMPDIR/ledger -print0 | xargs -0 chmod u+w
    rm -fr $TMPDIR/ledger || exit 1
fi

cd $TMPDIR || exit 1
mkdir ledger || exit 1
cd ledger || exit 1

# Pull the Ledger sources from the Subversion repository.

LEDGER_SVN=https://ledger.svn.sourceforge.net/svnroot/ledger

if [ -d $HOME/Projects/ledger ]; then
    cp -Rp $HOME/Projects/ledger local_svn
fi

# Create a reference copy of the sources in a pristine working tree
# that will not be modified.  Copies are made from this copy to avoid
# having to go to the network each time.  The function
# `dup_working_tree' creates a copy for us, either cheaply using git,
# or via an ordinary copy if we're using subversion.

svn checkout $LEDGER_SVN/trunk local_svn

function dup_working_tree() {
    cp -Rp local_svn "$1" || exit 1
}

# These functions understand how to do a distcheck build for ledger
# completely from scratch.

function build_distcheck_from_scratch() {
    cd $TMPDIR/ledger || exit 1
    dup_working_tree distcheck_scratch || exit 1
    cd distcheck_scratch || exit 1
    ./acprep --local || exit 1
    make CPPFLAGS="$MY_CPPFLAGS" LDFLAGS="$MY_LDFLAGS" distcheck || exit 1
}

# Finally, we have the ordinary `build_ledger' function, which builds
# ledger from scratch using whichever acprep arguments have been
# passed in.

function build_ledger() {
    name=$1
    shift 1

    cd $TMPDIR/ledger || exit 1
    dup_working_tree $name || exit 1
    cd $name || exit 1

    ./acprep --local "$@" || exit 1

    (cd gdtoa && make) || exit 1
    make || exit 1

    if [ "$1" = "--opt" ]; then
	make check || exit 1
    else
	make fullcheck || exit 1
    fi
}

# With all of that defined, now build ledger in all its various
# flavors, do a "fullcheck" for each non-optimized one (which uses
# valgrind on Linux, and gmalloc on OS/X), and a "check" for each
# optimized one.  Note that this will take a long while!

build_distcheck_from_scratch || exit 1

build_ledger normal || exit 1
build_ledger python --python || exit 1

build_ledger debug --debug || exit 1
build_ledger debug_python --debug --python || exit 1
#build_ledger boost_debug --debug --boost d || exit 1

build_ledger optimized --opt || exit 1
build_ledger opt_python --opt --python || exit 1

exit 0
