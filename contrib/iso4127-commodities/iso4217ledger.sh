#!/bin/sh

# iso4217ledger.sh - Convert ISO 4217 currencies to ledger commodities
#
# This script will download the latest XML for ISO 4217 Table A.1
# and print the contained currency & funds code list as ledger
# commodity definitions to stdout.

# Copyright (c) 2014 Alexis Hildebrandt
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

xml_url="http://www.currency-iso.org/dam/downloads/table_a1.xml"
xsl_file="$(dirname $0)/iso4217ledger.xsl"

xsltproc="$(which xsltproc)"
if [ ! -f "$xsltproc" -o ! -x "$xsltproc" ]; then
  echo "Can't find xsltproc"
  exit 1
fi

download_command="$(which curl)"
if [ -f "$download_command" \
  -a -x "$download_command" ]; then
  download_options="--silent"
else
  download_command="$(which wget)"
  if [ -n "$download_command" \
    -a -f "$download_command" \
    -a -x "$download_command" ]; then
    download_options="--quiet --output-document -"
  else
    echo "Can't find curl or wget."
    exit 1
  fi
fi

$download_command $download_options "$xml_url" | $xsltproc "$xsl_file" -
