#!/bin/sh

if [ -n "$1" ]; then
    ./ledger --seed=$1 --actual --args-only generate > /tmp/cout
else
    ./ledger --actual --args-only generate > /tmp/cout
fi

ledger -f /tmp/cout --actual --args-only print > /tmp/print

diff -w -U3 /tmp/cout /tmp/print
