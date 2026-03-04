#!/bin/sh
# Mock getquote script that returns price in wrong commodity (GBP instead of $)
# Used by coverage-commodity-mismatch.test to exercise commodity.cc line 212
SYMBOL="$1"
echo "$SYMBOL GBP0.85"
