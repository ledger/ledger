#!/usr/bin/perl
# fund-report.plx                                    -*- Perl -*-
#
#    Script to generate a Restricted Fund Report.  Usefulness of this
#    script may be confined to those who track separate funds in their
#    accounts by having accounts that match this format:
#     /^(Income|Expenses|Unearned Income|(Accrued:[^:]+:):PROJECTNAME/

#  Conservancy does this because we carefully track fund balances for our
#  fiscal sponsored projects.  Those who aren't fiscal sponsors won't find
#  this report all that useful, I suspect.  Note that the name
#  "Conservancy" is special-cased in a few places, mainly because our
#  "General" fund is called "Conservancy".

#
# Copyright (C) 2011, 2012, 2013 Bradley M. Kuhn
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

my $LEDGER_CMD = "/usr/local/bin/ledger";

my $ACCT_WIDTH = 70;

sub ParseNumber($) {
  $_[0] =~ s/,//g;
  return Math::BigFloat->new($_[0]);
}
Math::BigFloat->precision(-2);
my $ZERO =  Math::BigFloat->new("0.00");
my $TWO_CENTS =  Math::BigFloat->new("0.02");

if (@ARGV < 2) {
  print STDERR "usage: $0 <START_DATE> <END_DATE> <LEDGER_OPTIONS>\n";
  exit 1;
}
my($startDate, $endDate, @mainLedgerOptions) = @ARGV;

my $err;
my $formattedEndDate = UnixDate(DateCalc(ParseDate($endDate), ParseDateDelta("- 1 day"), \$err),
                                "%Y/%m/%d");
die "Date calculation error on $endDate" if ($err);
my $formattedStartDate = UnixDate(ParseDate($startDate), "%Y/%m/%d");
die "Date calculation error on $startDate" if ($err);

# First, get balances for starting and ending for each fund

my %funds;

foreach my $type ('starting', 'ending') {
  my(@ledgerOptions) = (@mainLedgerOptions,
                      '-V', '-X', '$', '-F', "%-.70A %22.108t\n", '-s');

  if ($type eq 'starting') {
    push(@ledgerOptions, '-e', $startDate);
  } else {
    push(@ledgerOptions,'-e', $endDate);
  }
  push(@ledgerOptions, 'reg', '/^(Income|Expenses):([^:]+):/');

  open(LEDGER_FUNDS, "-|", $LEDGER_CMD, @ledgerOptions)
    or die "Unable to run $LEDGER_CMD @ledgerOptions: $!";

  while (my $fundLine = <LEDGER_FUNDS>) {
    die "Unable to parse output line from first funds command: \"$fundLine\""
      unless $fundLine =~ /^\s*([^\$]+)\s+\$\s*\s*([\-\d\.\,]+)/;
    my($account, $amount) = ($1, $2);
    $amount = ParseNumber($amount);
    $account =~ s/\s+$//;
    next if $account =~ /\<Adjustment\>/ and (abs($amount) <= $TWO_CENTS);
    die "Weird account found, $account with amount of $amount in command: @ledgerOptions\n"
      unless $account =~ s/^\s*(?:Income|Expenses):([^:]+)://;
    $account = $1;
    $account = 'General' if $account eq 'Conservancy';   # FIXME: this is a special case for Consrevancy
    $funds{$account}{$type} += $amount;
  }
  close LEDGER_FUNDS;
  die "Failure on ledger command @ledgerOptions: $!" unless ($? == 0);
}
foreach my $fund (keys %funds) {
  foreach my $type (keys %{$funds{$fund}}) {
    $funds{$fund}{$type} = $ZERO - $funds{$fund}{$type};
  }
}
my(@ledgerOptions) = (@mainLedgerOptions,
                  '-V', '-X', '$', '-F', "%-.70A %22.108t\n",  '-w', '-s',
                  '-b', $startDate, '-e', $endDate, 'reg');

my @possibleTypes = ('Income', 'Expenses', 'Unearned Income', 'Retained Earnings', 'Retained Costs',
                     'Accrued:Loans Receivable', 'Accrued:Accounts Payable',
                     'Accrued:Accounts Receivable', 'Accrued:Expenses');

foreach my $type (@possibleTypes) {
  foreach my $fund (keys %funds) {
    my $query;
    $query = ($fund eq 'General') ? "/^${type}:Conservancy/": "/^${type}:$fund/";
    open(LEDGER_INCOME, "-|", $LEDGER_CMD, @ledgerOptions, $query)
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
    die "Failure on ledger command for ${type}:$fund: $!" unless ($? == 0);
  }
}

my %tot;
($tot{Start}, $tot{End}) = ($ZERO, $ZERO);

my %beforeEndings = ('Income' => 1, 'Expenses' => 1);
my %afterEndings;

# For other @possibleTypes, build up @fieldsList to just thoes that are present. 

foreach my $fund (keys %funds) {
  foreach my $type (@possibleTypes) {
    if ($funds{$fund}{$type} != $ZERO) {
      if ($type =~ /^(Unearned Income|Accrued)/) {
        $afterEndings{$type} = 1;
      } else {
        $beforeEndings{$type} = 1;
      }
    }
  }
}
my(@beforeEndingFields, @afterEndingFields);

foreach my $ii (@possibleTypes) {
  push(@beforeEndingFields, $ii) if defined $beforeEndings{$ii};
  push(@afterEndingFields, $ii)  if defined $afterEndings{$ii};
}
# Make sure fieldLists present items are zero for those that should be zero.
foreach my $fund (keys %funds) {
  foreach my $type ('starting', @beforeEndingFields, 'ending', @afterEndingFields) {
    $funds{$fund}{$type} = $ZERO unless defined $funds{$fund}{$type};
  }
}

print '"RESTRICTED AND GENERAL FUND REPORT",', "\"BEGINNING:\",\"$formattedStartDate\",\"ENDING:\",\"$formattedEndDate\"\n\n";
print '"FUND","STARTING BALANCE",';
my @finalPrints;
foreach my $type (@beforeEndingFields) {
  $tot{$type} = $ZERO;
  my $formattedType = $type;
  print "\"$formattedType\",";
}
print '"ENDING BALANCE",""';
foreach my $type (@afterEndingFields) {
  $tot{$type} = $ZERO;
  my $formattedType = $type;
  $formattedType = "Prepaid Expenses" if $formattedType eq 'Accrued:Expenses'; 
  $formattedType =~ s/^Accrued://;
  print ",\"$formattedType\"";
}
print "\n\n";

sub printTotal ($$) {
  my($label, $tot) = @_;
  print "\"$label\",\"\$$tot->{Start}\",";
  foreach my $type (@beforeEndingFields) {
    print "\"\$$tot->{$type}\",";
  }
  print "\"\$$tot->{End}\",\"\"";
  foreach my $type (@afterEndingFields) {
    print ",\"\$$tot->{$type}\"";
  }
  print "\n";
}

foreach my $fund (sort {
                        if ($a eq "General") { return 1 }
                        elsif ($b eq "General") { return -1 }
                        else { return $a cmp $b } }
                  keys %funds) {
  my $sanityTotal = $funds{$fund}{starting};

  if ($fund eq 'General') {
    print "\n";
    printTotal("Restricted Subtotal", \%tot);
    print "\n";
  }
  $tot{Start} += $funds{$fund}{starting};
  $tot{End} += $funds{$fund}{ending};

  print "\"$fund\",\"\$$funds{$fund}{starting}\",";
  foreach my $type (@beforeEndingFields) {
    print "\"\$$funds{$fund}{$type}\",";
    $tot{$type} += $funds{$fund}{$type};
  }
  print "\"\$$funds{$fund}{ending}\",\"\"";
  foreach my $type (@afterEndingFields) {
    print ",\"\$$funds{$fund}{$type}\"";
    $tot{$type} += $funds{$fund}{$type};
  }
  print "\n";
  # Santity check:
  if (abs($funds{$fund}{ending} -
      ($funds{$fund}{starting}
         - $funds{$fund}{Income} - $funds{$fund}{Expenses}))
      > $TWO_CENTS) {
    print "$fund FAILED SANITY CHECK: Ending: $funds{$fund}{ending} \n\n\n";
    warn "$fund FAILED SANITY CHECK";
  }
}
print "\n";
printTotal("OVERALL TOTAL", \%tot);
###############################################################################
#
# Local variables:
# compile-command: "perl -c fund-report.plx"
# End:

