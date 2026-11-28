#!/bin/sh
# Mock getquote script for testing quotes.cc
# Arguments: $1 = commodity symbol, $2 = exchange commodity symbol
# Returns a price directive line
# Batch mode (--batch) is refused, so Ledger asks for one quote at a time.

SYMBOL="$1"
EXCHANGE="$2"

case "$SYMBOL" in
    --batch)
        exit 1
        ;;
    AAPL)
        echo "2024/06/15 $SYMBOL \$150.00"
        ;;
    BADQUOTE)
        exit 1
        ;;
    EMPTYQUOTE)
        # Return nothing
        ;;
    *)
        echo "2024/06/15 $SYMBOL \$100.00"
        ;;
esac
