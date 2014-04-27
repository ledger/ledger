#!/bin/bash

# By default US Letter is used as the PDF papersize.
# For those preferring other dimensions add a4 or small
# as a commandline argument to this script to create a
# DIN A4 or smallbook version of the PDF.
case $1 in
  a4*|afour*)
    papersize='--texinfo=@afourpaper';;
  small*)
    papersize='--texinfo=@smallbook';;
  *)
    papersize='';; # US Letter is texinfo default
esac

echo "===================================== Making Info..."
makeinfo ledger3.texi
echo "===================================== Making HTML..."
makeinfo --html --no-split ledger3.texi
echo "===================================== Making PDF..."
texi2pdf --quiet --batch ${papersize} ledger3.texi
