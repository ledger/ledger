#!/usr/bin/perl
# cash-receipts-and-disbursments-journals                                    -*- Perl -*-
#
#    Script to generate a General Ledger report that accountants like
#    using Ledger.
#
# Copyright (C) 2011, 2012 Bradley M. Kuhn
#
# This program gives you software freedom; you can copy, modify, convey,
# and/or redistribute it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program in a file called 'GPLv3'.  If not, write to the:
#    Free Software Foundation, Inc., 51 Franklin St, Fifth Floor
#                                    Boston, MA 02110-1301, USA.

use strict;
use warnings;

use Math::BigFloat;
use Date::Manip;
use File::Temp qw/tempfile/;

my $LEDGER_CMD = "/usr/local/bin/ledger";

my $ACCT_WIDTH = 75;

sub ParseNumber($) {
  $_[0] =~ s/,//g;
  return Math::BigFloat->new($_[0]);
}

sub LedgerAcctToFilename($) {
  my $x = $_[0];
  $x =~ s/ /-/g;
  $x =~ s/:/-/g;
  return $x;
}

Math::BigFloat->precision(-2);
my $ZERO =  Math::BigFloat->new("0.00");

if (@ARGV < 2) {
  print STDERR "usage: $0 <BEGIN_DATE> <END_DATE> <OTHER_LEDGER_OPTS>\n";
  exit 1;
}

my($beginDate, $endDate, @otherLedgerOpts) = @ARGV;

my(@chartOfAccountsOpts) = ('-V', '-F', "%150A\n",  '-w', '-s',
                            '-b', $beginDate, '-e', $endDate, @otherLedgerOpts, 'reg');

open(CHART_DATA, "-|", $LEDGER_CMD, @chartOfAccountsOpts)
  or die "Unable to run $LEDGER_CMD @chartOfAccountsOpts: $!";

my @accounts;
while (my $line = <CHART_DATA>) {
  chomp $line;
  $line =~ s/^\s*//;   $line =~ s/\s*$//;
  push(@accounts, $line);

}
close(CHART_DATA); die "error reading ledger output for chart of accounts: $!" unless $? == 0;

my $formattedEndDate = new Date::Manip::Date;
die "badly formatted end date, $endDate" if $formattedEndDate->parse($endDate);
my $oneDayLess = new Date::Manip::Delta;
die "bad one day less" if $oneDayLess->parse("- 1 day");
$formattedEndDate = $formattedEndDate->calc($oneDayLess);
$formattedEndDate = $formattedEndDate->printf("%Y/%m/%d");

foreach my $acct (@accounts) {
  next unless ($acct =~ /^(?:Assets|Liabilities)/);

  my $acctFilename = LedgerAcctToFilename($acct);

  foreach my $typeData ({ name => 'disbursements', query => 'a<=0' },
                         { name => 'receipts', query => 'a>0' }) {
    my $fileNameBase = $acctFilename . '-' . $typeData->{name};

    open(TEXT_OUT, ">", "$fileNameBase.txt") or die "unable to open $fileNameBase.txt: $!";
    open(CSV_OUT, ">", "$fileNameBase.csv") or die "unable to open $fileNameBase.csv: $!";

    print TEXT_OUT "\n\nACCOUNT: $acct\nFROM:    $beginDate TO $formattedEndDate\n\n";
    print CSV_OUT "\n\"ACCOUNT:\",\"$acct\"\n\"PERIOD START:\",\"$beginDate\"\n\"PERIOD END:\",\"$formattedEndDate\"\n";
    print CSV_OUT '"DATE","CHECK NUM","NAME","ACCOUNT","AMOUNT"';

    my @entryLedgerOpts = ('-l', $typeData->{query},
                           '-b', $beginDate, '-e', $endDate, @otherLedgerOpts, 'print', $acct);

    open(ENTRY_DATA, "-|", $LEDGER_CMD, @entryLedgerOpts)
      or die "Unable to run $LEDGER_CMD @entryLedgerOpts: $!";

    my($tempFH, $tempFile) = tempfile("cashreportsXXXXXXXX", TMPDIR => 1);

    while (my $line = <ENTRY_DATA>) { print $tempFH $line; }
    close(ENTRY_DATA); die "Error reading ledger output for entries: $!" unless $? == 0;
    $tempFH->close() or die "Error writing ledger output for entries to temp file, $tempFile: $!";

    goto SKIP_REGISTER_COMMANDS if (-z $tempFile);

    my @txtRegLedgerOpts = ('-f', $tempFile, '-V', '-F',
                            "%(date)  %-.70P  %-.10C  %-.80A  %18t\n", '-w', '--sort', 'd',
                            '-b', $beginDate, '-e', $endDate, 'reg');

    my $formatString = '\n"%(date)","%C","%P","%A","%t"\n%/"","","","%A","%t"';
    foreach my $tagField (qw/Receipt Invoice Statement Contract PurchaseOrder Approval Check IncomeDistributionAnalysis CurrencyRate/) {
      print CSV_OUT ',"', $tagField, '"';
      $formatString .= ',"link:%(tag(\'' . $tagField . '\'))"';
    }
    $formatString .= "\n";
    print CSV_OUT "\n";
    my @csvRegLedgerOpts = ('-f', $tempFile, '-V', '-F', $formatString, '-w', '--sort', 'd',
                            '-b', $beginDate, '-e', $endDate, 'reg');


    open(TXT_DATA, "-|", $LEDGER_CMD, @txtRegLedgerOpts)
      or die "unable to run ledger command for $fileNameBase.txt: $!";

    while (my $line = <TXT_DATA>) { print TEXT_OUT $line; }
    close(TEXT_OUT); die "Error read write text out to $fileNameBase.txt: $!" unless $? == 0;

    open(CSV_DATA, "-|", $LEDGER_CMD, @csvRegLedgerOpts)
      or die "unable to run ledger command for $fileNameBase.csv: $!";

    while (my $line = <CSV_DATA>) { $line =~ s/"link:"/""/g; print CSV_OUT $line; }
    close(CSV_DATA); die "Error read from csv ledger command $!" unless $? == 0;

  SKIP_REGISTER_COMMANDS:
    close(TXT_DATA); die "Error read from txt ledger command $!" unless $? == 0;
    close(CSV_OUT); die "Error read write csv out to $fileNameBase.csv: $!" unless $? == 0;
    unlink($tempFile);
  }
}
###############################################################################
#
# Local variables:
# compile-command: "perl -c cash-receipts-and-disbursments-journals.plx"
# End:

