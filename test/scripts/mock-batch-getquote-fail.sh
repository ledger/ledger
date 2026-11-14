#!/bin/sh
# Mock getquote script whose batch mode prints prices and then fails.
# Used by test/regress/getquote-batch-failure.test.
#
# Batch mode: $1 = --batch, $2 = exchange, $3.. = symbols.  Prints a
# $999.00 price dated 2024/06/16 for every symbol, and a line pricing $ in
# $, which Ledger rejects if it ever parses it, then exits 1: a failed batch
# has fetched nothing, so none of this output may be read.
# Single mode: $1 = symbol, $2 = exchange.  Prints the real price, dated a
# day earlier than the batch prices.

if [ "$1" = "--batch" ]; then
  shift
  shift
  for SYMBOL in "$@"; do
    echo "2024/06/16 $SYMBOL \$999.00"
  done
  echo "2024/06/16 \$ \$1.00"
  exit 1
fi

case "$1" in
  AAPL)
    echo "2024/06/15 AAPL \$150.00"
    ;;
  GOOG)
    echo "2024/06/15 GOOG \$180.00"
    ;;
  *)
    echo "2024/06/15 $1 \$100.00"
    ;;
esac
