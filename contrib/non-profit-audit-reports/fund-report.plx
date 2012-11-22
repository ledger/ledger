#!/usr/bin/perl
# fund-report.plx                                    -*- Perl -*-
#
#    Script to generate a Trial Balance report for a ledger.
#
# Copyright (C) 2011, 2012, Bradley M. Kuhn
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

my $LEDGER_CMD = "/usr/local/bin/ledger";

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
                      '-V', '-X', '$', '-F', "%-.70A %22.108t\n", '-s',
                      '-e', $endDate, 'reg', '/^Funds:Restricted:/');
my %funds;

open(LEDGER_FUNDS, "-|", $LEDGER_CMD, @ledgerOptions)
  or die "Unable to run $LEDGER_CMD for funds: $!";

while (my $fundLine = <LEDGER_FUNDS>) {
  die "Unable to parse output line from first funds command: \"$fundLine\""
    unless $fundLine =~ /^\s*([^\$]+)\s+\$\s*\s*([\-\d\.\,]+)/;
  my($account, $amount) = ($1, $2);
  $amount = ParseNumber($amount);
  $account =~ s/\s+$//;
  next if $account =~ /\<Adjustment\>/ and (abs($amount) <= 0.02);
  die "Weird account found, $account with amount of $amount in first funds command\n"
    unless $account =~ s/^\s*Funds:Restricted://;
  $funds{$account}{ending} = $amount;
}
close LEDGER_FUNDS;

# First, get fund list from starting balance
@ledgerOptions = (@mainLedgerOptions,
                  '-V', '-X', '$', '-F', "%-.70A %22.108t\n",  '-w', '-s',
                  '-e', $startDate, 'reg', '^Funds:Restricted:');

open(LEDGER_FUNDS, "-|", $LEDGER_CMD, @ledgerOptions)
  or die "Unable to run $LEDGER_CMD for funds: $!";

while (my $fundLine = <LEDGER_FUNDS>) {
  die "Unable to parse output line from second funds command: $fundLine"
    unless $fundLine =~ /^\s*([^\$]+)\s+\$\s*([\-\d\.\,]+)/;
  my($account, $amount) = ($1, $2);
  $amount = ParseNumber($amount);
  $account =~ s/\s+$//;
  next if $account =~ /\<Adjustment\>/ and (abs($amount) <= 0.02);
  die "Weird account found, $account with amount of $amount in first second command\n"
    unless $account =~ s/^\s*Funds:Restricted://;
  $funds{$account}{starting} = $amount;
}
close LEDGER_FUNDS;


foreach my $fund (keys %funds) {
  $funds{$fund}{starting} = $ZERO if not defined $funds{$fund}{starting};
}

@ledgerOptions = (@mainLedgerOptions,
                  '-V', '-X', '$', '-F', "%-.70A %22.108t\n",  '-w', '-s',
                  '-b', $startDate, '-e', $endDate, 'reg');

my @possibleTypes = ('Unearned Income', 'Retained Earnings', 'Retained Costs',
                     'Accrued:Accounts Payable', 'Accrued:Accounts Receivable');

foreach my $type ('Income', 'Expenses', @possibleTypes) {
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
  my $sanityTotal = $funds{$fund}{starting};
  print "Fund: $fund\n", sprintf("%-35s\$%26.2f\n\n", "Balance as of $startDate:",
      $funds{$fund}{starting});
  foreach my $type ('Income', 'Expenses', @possibleTypes) {
    my $formattedType = $type;   $formattedType =~ s/^Accrued://;
    next if $type ne 'Income' and $type ne 'Expenses' and $funds{$fund}{$type} == $ZERO;
    print sprintf("%19s during period: \$%26.2f\n", $formattedType, $funds{$fund}{$type});
  }
  print sprintf("\n%-35s\$%26.2f\n", "Balance as of $endDate:",
      $funds{$fund}{ending}), "\n\n";
  # Santity check:
  if ($funds{$fund}{ending} !=
      ($funds{$fund}{starting}
         - $funds{$fund}{Income} - $funds{$fund}{'Unearned Income'} - $funds{$fund}{Expenses})) {
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

