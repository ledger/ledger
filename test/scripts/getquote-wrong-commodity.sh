#!/bin/sh
# Mock getquote script that returns price in wrong commodity (GBP instead of $)
# Used by coverage-commodity-mismatch.test to exercise commodity.cc line 212
SYMBOL="$1"
# Batch mode is refused, so Ledger asks for one quote at a time.
if [ "$SYMBOL" = "--batch" ]; then exit 1; fi
echo "$SYMBOL GBP0.85"
