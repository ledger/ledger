#!/bin/bash
echo "===================================== Making Info..."
makeinfo ledger3.texi
echo "===================================== Making PDF..."
texi2pdf --quiet --batch ledger3.texi
