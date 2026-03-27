#!/bin/sh
# Mock getquote script that supports both single and batch mode.
# Used by test/regress/588.test to verify batch quote fetching.
#
# Single mode: $1 = commodity symbol, $2 = exchange commodity
# Batch mode:  $1 = --batch, $2 = exchange commodity, $3.. = commodity symbols
#
# Returns one price directive line per commodity.

if [ "$1" = "--batch" ]; then
  shift
  EXCHANGE="$1"
  shift
  for SYMBOL in "$@"; do
    case "$SYMBOL" in
      AAPL)
        echo "2024/06/15 AAPL \$150.00"
        ;;
      GOOG)
        echo "2024/06/15 GOOG \$180.00"
        ;;
      MSFT)
        echo "2024/06/15 MSFT \$420.00"
        ;;
      *)
        echo "2024/06/15 $SYMBOL \$100.00"
        ;;
    esac
  done
else
  SYMBOL="$1"
  EXCHANGE="$2"
  case "$SYMBOL" in
    AAPL)
      echo "2024/06/15 AAPL \$150.00"
      ;;
    GOOG)
      echo "2024/06/15 GOOG \$180.00"
      ;;
    MSFT)
      echo "2024/06/15 MSFT \$420.00"
      ;;
    *)
      echo "2024/06/15 $SYMBOL \$100.00"
      ;;
  esac
fi
