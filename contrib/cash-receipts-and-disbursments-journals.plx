#!/usr/bin/perl
# cash-receipts-and-disbursments-journals                                    -*- Perl -*-
#
# Copyright (C) 2011, Bradley M. Kuhn
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# - Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.

# - Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

use strict;
use warnings;

use Math::BigFloat;
use Date::Manip;
use File::Temp qw/tempfile/;

my $LEDGER_CMD = "/usr/bin/ledger";

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

my(@chartOfAccountsOpts) = ('--wide-register-format', "%150A\n",  '-w', '-s',
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
    print CSV_OUT '"DATE","CHECK NUM","NAME","ACCOUNT","AMOUNT"', "\n";

    my @entryLedgerOpts = ('-l', $typeData->{query},
                           '-b', $beginDate, '-e', $endDate, @otherLedgerOpts, 'print', $acct);

    open(ENTRY_DATA, "-|", $LEDGER_CMD, @entryLedgerOpts)
      or die "Unable to run $LEDGER_CMD @entryLedgerOpts: $!";

    my($tempFH, $tempFile) = tempfile("cashreportsXXXXXXXX", TMPDIR => 1);

    while (my $line = <ENTRY_DATA>) { print $tempFH $line; }
    close(ENTRY_DATA); die "Error reading ledger output for entries: $!" unless $? == 0;
    $tempFH->close() or die "Error writing ledger output for entries to temp file, $tempFile: $!";

    goto SKIP_REGISTER_COMMANDS if (-z $tempFile);

    my @txtRegLedgerOpts = ('-f', $tempFile, '--wide-register-format',
                            "%D  %-.70P  %-.10C  %-.80A  %18t\n%/%68|%15|%-.80A  %18t\n", '-w', '--sort', 'd',
                            '-b', $beginDate, '-e', $endDate, 'reg');

    my @csvRegLedgerOpts = ('-f', $tempFile, '--wide-register-format',
                            '\n"%D","%C","%P","%A","%t"\n%/"","","","%A","%t"\n', '-w', '--sort', 'd',
                            '-b', $beginDate, '-e', $endDate, 'reg');


    open(TXT_DATA, "-|", $LEDGER_CMD, @txtRegLedgerOpts)
      or die "unable to run ledger command for $fileNameBase.txt: $!";

    while (my $line = <TXT_DATA>) { print TEXT_OUT $line; }
    close(TEXT_OUT); die "Error read write text out to $fileNameBase.txt: $!" unless $? == 0;

    open(CSV_DATA, "-|", $LEDGER_CMD, @csvRegLedgerOpts)
      or die "unable to run ledger command for $fileNameBase.csv: $!";

    while (my $line = <CSV_DATA>) { print CSV_OUT $line; }
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

