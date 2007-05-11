#!/bin/bash

TMPDIR=$HOME/tmp

if [ -d $TMPDIR/ledger ]; then
    rm -fr $TMPDIR/ledger || exit 1
fi

if [ -d $HOME/src/ledger/.git ]; then
    LEDGER_GIT=$HOME/src/ledger
else
    LEDGER_GIT=http://newartisans.com/ledger.git
fi

cd $TMPDIR

mkdir ledger || exit 1

cd ledger
git clone $LEDGER_GIT local_git || exit 1

git clone -l local_git distcheck || exit 1
cd distcheck || exit 1
./acprep --local || exit 1
make CPPFLAGS="-I/usr/local/include -I/usr/local/include/boost -I/sw/include" \
     LDFLAGS="-L/usr/local/lib -L/sw/lib" distcheck || exit 1

function build_ledger() {
    name=$1
    shift 1

    cd $TMDIR/ledger || exit 1
    git clone -l local_git $name || exit 1
    cd $name || exit 1

    ./acprep --local "$@" || exit 1

    (cd gdtoa && make) || exit 1
    make || exit 1
    make fullcheck || exit 1
}

build_ledger(normal)
build_ledger(devel, --devel)
build_ledger(python, --python)

build_ledger(debug, --debug)
build_ledger(boost_debug, --debug, --boost, d)
build_ledger(debug_python, --debug, --python)

build_ledger(optimized, --opt)
build_ledger(opt_python, --opt, --python)

rm -fr $TMPDIR/ledger || exit 1

exit 0
