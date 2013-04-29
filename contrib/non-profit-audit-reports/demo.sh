#!/bin/sh
# demo.sh
# Demonstrate a non-profit GL export and conversion to ODS

program=$(basename $0)
dir=$(dirname $0)
cd $dir
dir=$(pwd -P)
export PYTHONPATH=$dir/ooolib2

getcsv=$dir/general-ledger-report.plx
csv2ods=$dir/csv2ods.py

echo "Demonstrating ledger to ODS export in $dir/tests"
cd $dir/tests
sampledata=non-profit-test-data.ledger
echo "  based on the sample data in $sampledata"

$getcsv 2011/03/01 2012/03/01 -f $sampledata
if [ -e general-ledger.csv ]; then
  echo "data was exported to: general-ledger.csv"
else
  echo "error creating csv file"
  exit 1
fi

$csv2ods --verbose --csv general-ledger.csv
if [ -e general-ledger.ods ]; then
  echo "csv was converted to: general-ledger.ods"
else
  echo "error creating ods file"
  exit 1
fi

echo general-ledger.ods >> MANIFEST

# create a portable zip file with the spreadsheet
# and the linked artifacts

echo creating portable zipfile...
cat MANIFEST | zip -@ ../general-ledger.zip

echo " "
echo "created general-ledger.zip"

