#!/usr/bin/perl
# fund-report.plx                                    -*- Perl -*-
#
#    Script to generate a Trial Balance report for a ledger.
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

my $LEDGER_CMD = "/usr/bin/ledger";

my $ACCT_WIDTH = 70;

sub ParseNumber($) {
  $_[0] =~ s/,//g;
  return Math::BigFloat->new($_[0]);
}
Math::BigFloat->precision(-2);
my $ZERO =  Math::BigFloat->new("0.00");

if (@ARGV < 2) {
  print STDERR "usage: $0 <START_DATE> <END_DATE> <LEDGER_OPTIONS>\n";
  exit 1;
}

my($startDate, $endDate, @mainLedgerOptions) = @ARGV;

# First, get fund list from ending balance
my(@ledgerOptions) = (@mainLedgerOptions,
                      '--wide-register-format', "%-.70A %22.108t\n",  '-w', '-s',
                      '-e', $endDate, 'reg', '^Funds:Restricted:');


my %funds;

open(LEDGER_FUNDS, "-|", $LEDGER_CMD, @ledgerOptions)
  or die "Unable to run $LEDGER_CMD for funds: $!";

while (my $fundLine = <LEDGER_FUNDS>) {
  die "Unable to parse output line from funds command: $fundLine"
    unless $fundLine =~ /^\s*([^\$]+)\s+\$\s*\s*([\d\.\,]+)/;
  my($account, $amount) = ($1, $2);
  $amount = ParseNumber($amount);
  $account =~ s/^\s*Funds:Restricted://;    $account =~ s/\s+$//;
  $funds{$account}{ending} = $amount;
}
close LEDGER_FUNDS;

# First, get fund list from ending balance
@ledgerOptions = (@mainLedgerOptions,
                  '--wide-register-format', "%-.70A %22.108t\n",  '-w', '-s',
                  '-e', $startDate, 'reg', '^Funds:Restricted:');

open(LEDGER_FUNDS, "-|", $LEDGER_CMD, @ledgerOptions)
  or die "Unable to run $LEDGER_CMD for funds: $!";

while (my $fundLine = <LEDGER_FUNDS>) {
  die "Unable to parse output line from funds command: $fundLine"
    unless $fundLine =~ /^\s*([^\$]+)\s+\$\s*\s*([\d\.\,]+)/;
  my($account, $amount) = ($1, $2);
  $amount = ParseNumber($amount);
  $account =~ s/^\s*Funds:Restricted://;    $account =~ s/\s+$//;
  $funds{$account}{starting} = $amount;
}
close LEDGER_FUNDS;


foreach my $fund (keys %funds) {
  $funds{$fund}{starting} = $ZERO if not defined $funds{$fund}{starting};
}

@ledgerOptions = (@mainLedgerOptions,
                  '--wide-register-format', "%-.70A %22.108t\n",  '-w', '-s',
                  '-b', $startDate, '-e', $endDate, 'reg');

foreach my $type ('Income', 'Expenses') {
  foreach my $fund (keys %funds) {
    open(LEDGER_INCOME, "-|", $LEDGER_CMD, @ledgerOptions, "^${type}:$fund")
      or die "Unable to run $LEDGER_CMD for funds: $!";
      $funds{$fund}{$type} = $ZERO;
    while (my $line = <LEDGER_INCOME>) {
      die "Unable to parse output line from $type line command: $line"
        unless $line =~ /^\s*([^\$]+)\s+\$\s*\s*([\-\d\.\,]+)/;
      my($account, $amount) = ($1, $2);
      $amount = ParseNumber($amount);
      $funds{$fund}{$type} += $amount;
    }
    close LEDGER_INCOME;
  }
}

my($totStart, $totEnd) = ($ZERO, $ZERO);

foreach my $fund (sort keys %funds) {
  print "Fund: $fund\n";
  print "      Balance as of $startDate: ", sprintf("\$%15.2f\n\n", $funds{$fund}{starting});
  print "      Income during period:     ", sprintf("\$%15.2f\n", $funds{$fund}{Income});
  print "      Expenses during period:   ", sprintf("\$%15.2f\n\n", $funds{$fund}{Expenses});
  print "      Balance as of $endDate: ", sprintf("\$%15.2f\n", $funds{$fund}{ending});
  print "\n\n";
  # Santity check:
  if ($funds{$fund}{ending} !=
      ( ($funds{$fund}{starting} - $funds{$fund}{Income}) - $funds{$fund}{Expenses})) {
    print "$fund FAILED SANITY CHECK\n\n\n";
    die "$fund FAILED SANITY CHECK";
  }
  $totStart += $funds{$fund}{starting};
  $totEnd += $funds{$fund}{ending};
}
print "\n\n\nTotal Restricted Funds as of $startDate: ", sprintf("\$%15.2f\n", $totStart);
print "\nTotal Restricted Funds as of $endDate: ", sprintf("\$%15.2f\n", $totEnd);
###############################################################################
#
# Local variables:
# compile-command: "perl -c fund-report.plx"
# End:

