#!/bin/bash

/bin/rm -fr ~/Products/ledger/opt

./acprep -j16 opt update || exit 0

COMMIT=$(git describe --long --all)

SPEEDS=$(./acprep -j16 opt make speedtest 2>&1 \
         | grep "Finished executing command" \
         | awk '{print $1}' \
         | xargs)

echo $COMMIT,$(echo $SPEEDS | sed 's/ /,/g') >> ~/src/ledger/speed.log

exit 0
