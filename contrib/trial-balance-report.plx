#!/usr/bin/perl
# trail-balance-report.plx                                    -*- Perl -*-
#
#    Script to generate a Trial Balance report for a ledger.
#
# Copyright (C) 2011, Bradley M. Kuhn
#
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# - Redistributions of source code must retain the above copyright
#  notice, this list of conditions and the following disclaimer.
#
# - Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
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

# ledger --wide-register-format "%-.70A %22.108t\n" -f no-fund.ledger -b 2010/03/01 -e 2011/03/01 -w reg

use strict;
use warnings;

use Math::BigFloat;

my $LEDGER_CMD = "/usr/bin/ledger";

my $ACCT_WIDTH = 75;

# http://www.moneyinstructor.com/lesson/trialbalance.asp
# told me:

# Key to preparing a trial balance is making sure that all the account
# balances are listed under the correct column.  The appropriate columns
# are as follows:

# Assets = Debit balance
# Liabilities = Credit balance
# Expenses = Debit Balance
# Equity = Credit balance
# Revenue = Credit balance


# So, there are some sign switches that are needed:

sub ParseNumber($) {
  $_[0] =~ s/,//g;
  return Math::BigFloat->new($_[0]);
}

Math::BigFloat->precision(-2);
my $ZERO =  Math::BigFloat->new("0.00");

if (@ARGV == 0) {
  print STDERR "usage: $0 <LEDGER_OPTIONS>\n";
  exit 1;
}


my(@ledgerOptions) = ('--wide-register-format', "%-.${ACCT_WIDTH}A %22.108t\n",  '-w',
                      @ARGV, 'reg');

open(LEDGER_DATA, "-|", $LEDGER_CMD, @ledgerOptions)
  or die "Unable to run $LEDGER_CMD @ledgerOptions: $!";

my %acct;
while (my $line = <LEDGER_DATA>) {

  die "Unable to parse output line from negative_ledger command: $line"
    unless $line =~ /^\s*([^\$]+)\s+\$\s*\s*([\-\d\.\,]+)/;
  my($account, $amount) = ($1, $2);
  $amount = ParseNumber($amount);
  $account =~ s/^\s+//;    $account =~ s/\s+$//;
  $acct{$account}{debit} = $ZERO if not defined $acct{$account}{debit};
  $acct{$account}{credit} = $ZERO if not defined $acct{$account}{credit};

  if ($account =~ /^(Asset|Expense)/) {
      $acct{$account}{debit} += $amount;
  } elsif ($account =~ /^(Income)/) {
    $acct{$account}{debit} += - $amount;
  } elsif ($account =~ /^(Liabilities)/) {
    $acct{$account}{credit} += $amount;
  } else {
    die "unkown account type $account";
  }

}
close LEDGER_DATA;
die "error($0): $! while running ledger command line" unless ($? == 0);


print sprintf("%-${ACCT_WIDTH}.${ACCT_WIDTH}s        %s              %s\n\n", "ACCOUNT", "DEBITS", "CREDITS");

my $format = "%-${ACCT_WIDTH}.${ACCT_WIDTH}s       \$%11.2f       \$%11.2f\n";
my($totDeb, $totCred) = ($ZERO, $ZERO);
foreach my $account (sort keys %acct) {
  foreach my $val (qw/debit credit /) {
    $acct{$account}{$val} = $ZERO unless defined $acct{$account}{$val};
  }
  print sprintf($format, $account,
                $acct{$account}{debit},                 $acct{$account}{credit});
  $totDeb += $acct{$account}{debit};
  $totCred += $acct{$account}{credit};


}
print "\n\n", sprintf($format, 'TOTAL', $totDeb, $totCred);
###############################################################################
#
# Local variables:
# compile-command: "perl -c trial-balance-report.plx"
# End:

