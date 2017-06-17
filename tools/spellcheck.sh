#!/bin/sh

aspell --lang=en_US.UTF-8 check --mode=texinfo $(dirname $0)/../doc/ledger3.texi
