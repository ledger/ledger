#!/usr/bin/env bash

#set -x
set -e
set -o pipefail

if [ "${TRAVIS_OS_NAME}" = "osx" ]; then
  for formula in $(echo "${BREWS//,/ }"); do
    echo "Checking ${formula} formula"
    brew outdated "${formula}" \
      || (brew unlink "${formula}"
          brew install "${formula}"
         )
  done
fi

if [ -d "${BOOST_ROOT}" ]; then
  echo "Installing boost ${BOOST_VERSION} in ${BOOST_ROOT}"
  (cd "${BOOST_ROOT}"
    ./bootstrap.sh --with-libraries="${BOOST_LIBS}"
    ./b2 threading=multi --prefix="${BOOST_ROOT}" -d0 install
  )
fi
