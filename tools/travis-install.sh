#!/usr/bin/env bash

#set -x
set -e
set -o pipefail

if [ -d "${BOOST_ROOT}" ]; then
  (cd "${BOOST_ROOT}"
    ./bootstrap.sh --with-libraries="${BOOST_LIBS}"
    ./b2 threading=multi --prefix="${BOOST_ROOT}" -d0 install
  )
fi
