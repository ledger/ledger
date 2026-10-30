#!/bin/sh
# Mock getquote script for testing issue #3321.
# Prints a price line without a date, naming the commodity exactly as
# Ledger passed it in $1: in double quotes, when its symbol needs them.
# The line for "Fund A" begins with a space.  Batch mode is refused, so
# Ledger asks for one quote at a time.
#
# Arguments: $1 = commodity symbol, $2 = exchange commodity symbol

case "$1" in
  --batch)
    exit 1
    ;;
  '"Fund A"')
    echo " $1 \$31.00"
    ;;
  *)
    echo "$1 \$52.00"
    ;;
esac
