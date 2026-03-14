#!/bin/sh
# Mock getquote script for testing issue #1869.
# Verifies that ledger passes a non-empty exchange commodity as $2.
#
# When called with an empty exchange commodity (the bug), exits 1 (fail).
# When called with a non-empty exchange commodity (the fix), returns a
# price that differs from the P directive in the test journal so the
# regression test can distinguish the two cases.
#
# Arguments: $1 = commodity symbol, $2 = exchange commodity

SYMBOL="$1"
EXCHANGE="$2"

if [ -z "$EXCHANGE" ]; then
  # Bug: called without an exchange commodity -- signal failure so the
  # test balance does not match the expected (fixed) output.
  exit 1
fi

case "$SYMBOL" in
  BTC)
    echo "2024/06/15 BTC \$40000.00"
    ;;
  *)
    echo "2024/06/15 $SYMBOL \$100.00"
    ;;
esac
