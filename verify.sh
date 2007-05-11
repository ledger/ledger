#!/bin/bash

MY_CPPFLAGS="-I/usr/local/include -I/usr/local/include/boost -I/sw/include"
MY_LDFLAGS="-L/usr/local/lib -L/sw/lib"

# Setup the temporary directory where all these copies are ledger are
# going to be built.  Remove it if it's already there.

if [ -d $HOME/tmp ]; then
    TMPDIR=$HOME/tmp
else
    TMPDIR=/tmp
fi

if [ -d $TMPDIR/ledger ]; then
    sudo rm -fr $TMPDIR/ledger || exit 1
fi

cd $TMPDIR || exit 1
mkdir ledger || exit 1
cd ledger || exit 1

# Determine if we can use git to pull down Ledger, since it's very
# fast and efficient (and the trunk of ledger-git is more bleeding
# edge).  Otherwise, fall back on the public sebversion repository.

USING_GIT=true

cmd=$(which git 2>&1)
if [ ! -x "$cmd" ]; then
    USING_GIT=false
    LEDGER_SVN=https://ledger.svn.sourceforge.net/svnroot/ledger
elif [ -d $HOME/src/ledger/.git ]; then
    LEDGER_GIT=$HOME/src/ledger
else
    LEDGER_GIT=http://newartisans.com/ledger.git
fi

# Create a reference copy of the sources in a pristine working tree
# that will not be modified.  Copies are made from this copy to avoid
# having to go to the network each time.  The function
# `dup_working_tree' creates a copy for us, either cheaply using git,
# or via an ordinary copy if we're using subversion.

if [ "$USING_GIT" = "true" ]; then
    git clone $LEDGER_GIT local_git || exit 1
else
    svn checkout $LEDGER_SVN local_svn
fi

function dup_working_tree() {
    if [ "$USING_GIT" = "true" ]; then
	git clone -l local_git "$1" || exit 1
    else
	cp -Rp local_svn "$1" || exit 1
    fi
}

# These functions understand how to do a distcheck build for ledger
# either completely from scratch, or using the configure script that
# is maintained in the repository.

function build_distcheck_from_scratch() {
    dup_working_tree distcheck_scratch || exit 1
    cd distcheck_scratch || exit 1
    ./acprep --local || exit 1
    make CPPFLAGS="$MY_CPPFLAGS" LDFLAGS="$MY_LDFLAGS" distcheck || exit 1
}

function build_distcheck_from_distrib() {
    dup_working_tree distcheck_distrib || exit 1
    cd distcheck_distrib || exit 1
    ./configure CPPFLAGS="$MY_CPPFLAGS" LDFLAGS="$MY_LDFLAGS" || exit 1
    make distcheck || exit 1
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
    make fullcheck || exit 1
}

# With all of that defined, now build ledger in all its various
# flavors, do a "fullcheck" for each one (which uses valgrind on
# Linux, and gmalloc on OS/X).  Note that this will take a long while!

build_distcheck_from_scratch || exit 1
build_distcheck_from_distrib || exit 1

build_ledger normal || exit 1
build_ledger devel --devel || exit 1
build_ledger python --python || exit 1

build_ledger debug --debug || exit 1
#build_ledger boost_debug --debug --boost d || exit 1
build_ledger debug_python --debug --python || exit 1

build_ledger optimized --opt || exit 1
build_ledger opt_python --opt --python || exit 1

exit 0
