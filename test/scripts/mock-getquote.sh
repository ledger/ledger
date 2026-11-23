#!/bin/sh
# Mock getquote script for testing quotes.cc
# Single mode: $1 = commodity symbol, $2 = exchange commodity symbol
# Batch mode:  $1 = --batch, $2 = exchange commodity, $3.. = commodity symbols
# Returns a price directive line per commodity

quote_for() {
    case "$1" in
        AAPL)
            echo "2024/06/15 $1 \$150.00"
            ;;
        BADQUOTE)
            exit 1
            ;;
        EMPTYQUOTE)
            # Return nothing
            ;;
        BADLINE)
            # An unreadable price line (issue #3356)
            echo "2024/06/15 $1 N/A"
            ;;
        BAREWORD)
            # A bare word, not a price line at all (issue #3356)
            echo "N/A"
            ;;
        *)
            echo "2024/06/15 $1 \$100.00"
            ;;
    esac
}

if [ "$1" = "--batch" ]; then
    shift 2
    for SYMBOL in "$@"; do
        quote_for "$SYMBOL"
    done
else
    quote_for "$1"
fi
