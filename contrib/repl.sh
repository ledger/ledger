#!/bin/bash

EXEC=$(which ledger)
if [[ -z "$EXEC" ]]; then
   EXEC=$HOME/Products/ledger/ledger
fi

if [[ ! -x "$EXEC" ]]; then
   echo Cannot find Ledger executable
   exit 1
fi

LESS=--quit-if-one-screen exec $EXEC --pager less "$@"
