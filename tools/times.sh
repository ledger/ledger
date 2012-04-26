#!/bin/sh

time test/RegressTests.py ./ledger test/regress
time test/RegressTests.py ./ledger test/regress --verify
#time test/RegressTests.py ./ledger test/regress --gmalloc
#time test/RegressTests.py ./ledger test/regress --verify --gmalloc
