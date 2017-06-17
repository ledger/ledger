#!/bin/sh

flavor=$1
shift 1

JOBS=-j$(sysctl -n hw.activecpu)

OPTIONS="$flavor --debug --python --ninja $JOBS"
#OPTIONS="$flavor --debug --python --ninja --doxygen $JOBS"

time nice -n 20 nix-shell -p                            \
    cmake boost gmp mpfr libedit python texinfo gnused  \
    ninja doxygen                                       \
    --command "./acprep $OPTIONS make $JOBS $@" &&      \

time nice -n 20 nix-shell -p                            \
    cmake boost gmp mpfr libedit python texinfo gnused  \
    ninja doxygen                                       \
    --command "./acprep $OPTIONS check $JOBS $@"        \
                                                        \
