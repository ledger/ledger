#!/bin/sh
# Mock getquote script that answers for a symbol other than the one asked.
# Used by test/regress/getquote-pricedb-symbol.test.
#
# Batch mode is unsupported (exit 1), as in contrib/getquote.pl, so every
# quote comes from single mode: $1 = commodity symbol, $2 = exchange.
#   AAPL: one line, a price for GOOG only.
#   MSFT: a 5000-character line (too long to read, so skipped), a price for
#         GOOG, then a price for MSFT.

case "$1" in
  AAPL)
    echo "2030/06/15 GOOG \$100.00"
    ;;
  MSFT)
    printf '%05000d\n' 0
    echo "2030/06/15 GOOG \$101.00"
    echo "2030/06/15 MSFT \$40.00"
    ;;
  *)
    exit 1
    ;;
esac
