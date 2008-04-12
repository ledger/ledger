#!/bin/bash

VALGRIND=$(which valgrind 2>&1)

if [ -x "$VALGRIND" ]; then
    exec "$VALGRIND" --leak-check=full --show-reachable=yes "$@"
else
    exec "$@"
fi
