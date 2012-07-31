#!/bin/sh

flavor=$1
shift 1

JOBS=-j$(sysctl -n hw.activecpu)
OPTIONS="$flavor --debug --python --doxygen $JOBS"

time (                                          \
    cd ~/src/ledger ;                           \
    PATH=/usr/local/bin:/opt/local/bin:$PATH    \
      nice -n 20 ./acprep $OPTIONS make "$@" && \
    PATH=/usr/local/bin:/opt/local/bin:$PATH    \
      nice -n 20 ./acprep $OPTIONS check "$@"   \
)
