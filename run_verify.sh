#!/bin/bash

SRCDIR=$(dirname $0)

if [ -n "$1" -a -d "$1" ]; then
    TMPDIR="$1"
else
    TMPDIR=/tmp
fi

cd $TMPDIR || exit 1

cp -p "$SRCDIR"/verify.sh . || exit 1

./verify.sh > verify.out 2>&1 || (cat verify.out; exit 1)
