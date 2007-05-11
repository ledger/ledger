#!/bin/bash

# This script can be from cron to regularly verify that Ledger is
# sane.  Such a cron entry might look like this, assuming you keep a
# recent working tree in ~/src/ledger, and that you want all of the
# temporary build products kept in /tmp:
#
#   0 0 * * * $HOME/src/ledger/run_verify.sh /tmp
#
# Note that this script should be run as root!  Also, whether on
# success or failure the build log and build products are left in the
# temporary directory for later examination if desired.

SRCDIR=$(dirname $0)

if [ -n "$1" -a -d "$1" ]; then
    TMPDIR="$1"
else
    TMPDIR=/tmp
fi

cd $TMPDIR || exit 1

cp -p "$SRCDIR"/verify.sh . || exit 1

(./verify.sh > verify.out 2>&1 || (cat verify.out; exit 1))

rm -f verify.sh
