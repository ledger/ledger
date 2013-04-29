#!/bin/sh

flavor=$1
shift 1

JOBS=-j$(sysctl -n hw.activecpu)
OPTIONS="$flavor --debug --python --ninja --doxygen $JOBS"
OPTIONS="$OPTIONS --prefix /usr/local/Cellar/ledger/HEAD"

time (                                                  \
    cd ~/src/ledger ;                                   \
    PATH=/usr/local/bin:/opt/local/bin:$PATH            \
      nice -n 20 ./acprep $OPTIONS make $JOBS "$@" &&   \
    PATH=/usr/local/bin:/opt/local/bin:$PATH            \
      nice -n 20 ./acprep $OPTIONS check $JOBS "$@"     \
)
