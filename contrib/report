#!/bin/sh

# This script facilities plotting of a ledger register report.  If you
# use OS/X, and have AquaTerm installed, you will probably want to set
# LEDGER_TERM to "aqua".
#
# Examples of use:
#
#   report -j -M reg food            # plot monthly food costs
#   report -J reg checking           # plot checking account balance

if [ -z "$LEDGER_TERM" ]; then
  LEDGER_TERM="x11 persist"
fi

(cat <<EOF; ledger "$@") | gnuplot
  set terminal $LEDGER_TERM
  set xdata time
  set timefmt "%Y-%m-%d"
  plot "-" using 1:2 with lines
EOF
