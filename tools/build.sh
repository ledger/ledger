#!/bin/sh

flavor=$1
shift 1

time (                                                                          \
    cd ~/src/ledger ;                                                           \
    PATH=/usr/local/bin:/opt/local/bin:$PATH                                    \
      nice -n 20 ./acprep $flavor --debug --python --doxygen make "$@" &&       \
    PATH=/usr/local/bin:/opt/local/bin:$PATH                                    \
      nice -n 20 ./acprep $flavor --debug --python --doxygen check "$@"         \
)