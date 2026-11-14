#!/bin/sh
# Mock getquote that answers only the --batch protocol.  Used by the
# prefetch-quotes-* regression tests: a per-commodity call (no --batch)
# exits 1, so a commodity prefetch_quotes() leaves out shows up as a
# "Price quote script failed" warning and a stale value.  A pair this
# script does not know prints nothing.  EUR:AAPL answers only so that a
# request for EUR in terms of AAPL (the inverted edge of "P AAPL 100 EUR")
# would show up in the output.
#
# Batch mode: $1 = --batch, $2 = exchange commodity, $3.. = commodity symbols

if [ "$1" != "--batch" ]; then
  exit 1
fi
shift
EXCHANGE="$1"
shift
for SYMBOL in "$@"; do
  case "$SYMBOL:$EXCHANGE" in
    'AAPL:$') echo "2024/06/15 AAPL \$150.00" ;;
    'AAPL:EUR') echo "2024/06/15 AAPL 120 EUR" ;;
    'EUR:$') echo "2024/06/15 EUR \$1.20" ;;
    'EUR:AAPL') echo "2024/06/15 EUR 0.5 AAPL" ;;
    '$:AAPL') echo "2024/06/15 \$ 0.005 AAPL" ;;
  esac
done
