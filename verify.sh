#!/bin/bash

if [ -d $HOME/tmp ]; then
    TMPDIR=$HOME/tmp
else
    TMPDIR=/tmp
fi

if [ -d $TMPDIR/ledger ]; then
    sudo rm -fr $TMPDIR/ledger || exit 1
fi

if [ -d $HOME/src/ledger/.git ]; then
    LEDGER_GIT=$HOME/src/ledger
else
    LEDGER_GIT=http://newartisans.com/ledger.git
fi

cd $TMPDIR || exit 1
mkdir ledger || exit 1
cd ledger || exit 1
git clone $LEDGER_GIT local_git || exit 1

function build_distcheck() {
    git clone -l local_git distcheck || exit 1
    cd distcheck || exit 1
    ./acprep --local || exit 1
    make CPPFLAGS="-I/usr/local/include -I/usr/local/include/boost -I/sw/include" \
         LDFLAGS="-L/usr/local/lib -L/sw/lib" distcheck || exit 1
}

function build_ledger() {
    name=$1
    shift 1

    cd $TMPDIR/ledger || exit 1
    git clone -l local_git $name || exit 1
    cd $name || exit 1

    ./acprep --local "$@" || exit 1

    (cd gdtoa && make) || exit 1
    make || exit 1
    make fullcheck || exit 1
}

build_distcheck || exit 1

build_ledger normal || exit 1
build_ledger devel --devel || exit 1
build_ledger python --python || exit 1

build_ledger debug --debug || exit 1
#build_ledger boost_debug --debug --boost d || exit 1
build_ledger debug_python --debug --python || exit 1

build_ledger optimized --opt || exit 1
build_ledger opt_python --opt --python || exit 1

exit 0
