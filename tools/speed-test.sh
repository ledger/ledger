#!/bin/bash

set -e

cd ~/src/ledger
/bin/rm -fr ~/Products/ledger/opt
./acprep --no-python -j16 opt make check

COMMIT=$(git describe --long --all)

SPEEDS=$(./acprep --no-python -j16 opt make speedtest 2>&1 \
         | grep "Finished executing command" \
         | awk '{print $1}' \
         | xargs)

echo $COMMIT,$(echo $SPEEDS | sed 's/ /,/g') >> speed.log

exit 0
