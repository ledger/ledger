#!/usr/bin/perl
# external-accounts-total-reconcile.plx                                    -*- Perl -*-
#
#    Script to verify that balances listed in an external file all match
#    the balances
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
  my($val) = @_;
  $val =~ s/,//g;
  $val =~ s/\s+//g;
  $val = - $val if $val =~ s/^\s*\(//;

  return Math::BigFloat->new($val);
}


Math::BigFloat->precision(-2);
my $ZERO =  Math::BigFloat->new("0.00");

if (@ARGV < 3) {
  print STDERR "usage: $0 <START_DATE> <END_DATE> <OTHER_LEDGER_OPTS>\n";
  exit 1;
}

my($beginDate, $endDate, @otherLedgerOpts) = @ARGV;

my(@internalBalancesHistoricalOptions) = ('--wide-register-format', '%-.150A %22.108t\n',  '-w', '-s',
                            '-e', $endDate, @otherLedgerOpts, 'reg');

my(@internalBalancesPeriodOptions) = ('--wide-register-format', '%-.150A %22.108t\n',  '-w', '-s',
                            '-b', $beginDate, '-e', $endDate, @otherLedgerOpts, 'reg');

my %externalBalances;
while (my $line = <STDIN>) {
  chomp $line;
  $line =~ s/^\s*//;   $line =~ s/\s*$//;

  next unless $line =~
    /^\s*(\S+\:.+)\s+[\(\d].+\s+([\(?\s*\d\.\,]+)\s*\)?\s*$/;
  my($acct, $value) = ($1, $2);
  $acct =~ s/^\s*//;   $acct =~ s/\s*$//;
  $acct =~ s/\s{3,}[\(\)\d,\.\s]+$//;

  $externalBalances{$acct} = $ZERO if (not defined $externalBalances{$acct});
  $externalBalances{$acct} += ParseNumber($value);
}

open(ACCT_DATA, "-|", $LEDGER_CMD, @internalBalancesPeriodOptions)
  or die "Unable to run $LEDGER_CMD @internalBalancesPeriodOptions: $!";

my %internalBalancesPeriod;
while (my $line = <ACCT_DATA>) {
  chomp $line;
  $line =~ s/^\s*//;   $line =~ s/\s*$//;
  die "Strange line, \"$line\" found in ledger output" unless
    $line =~ /^\s*(\S+\:[^\$]+)\s+\$?\s*([\-\d\.\,]+)\s*$/;

  my($acct, $value) = ($1, $2);
  $acct =~ s/^\s*//;   $acct =~ s/\s*$//;

  $internalBalancesPeriod{$acct} = ParseNumber($value);

}
close(ACCT_DATA); die "error reading ledger output: $!" unless $? == 0;


open(ACCT_DATA, "-|", $LEDGER_CMD, @internalBalancesHistoricalOptions)
  or die "Unable to run $LEDGER_CMD @internalBalancesHistoricalOptions: $!";

my %internalBalancesHistorical;
while (my $line = <ACCT_DATA>) {
  chomp $line;
  $line =~ s/^\s*//;   $line =~ s/\s*$//;
  die "Strange line, \"$line\" found in ledger output" unless
    $line =~ /^\s*(\S+\:[^\$]+)\s+\$?\s*([\-\d\.\,]+)\s*$/;

  my($acct, $value) = ($1, $2);
  $acct =~ s/^\s*//;   $acct =~ s/\s*$//;

  $internalBalancesHistorical{$acct} = ParseNumber($value);

}
close(ACCT_DATA); die "error reading ledger output: $!" unless $? == 0;

my(@laterAccountOptions) = ('--wide-register-format', '%-.150A %22.108t\n',  '-w', '-s',
                            @otherLedgerOpts, 'reg');

open(LATER_ACCT_DATA, "-|", $LEDGER_CMD, @laterAccountOptions)
  or die "Unable to run $LEDGER_CMD @laterAccountOptions: $!";

my %laterInternalBalances;
while (my $line = <LATER_ACCT_DATA>) {
  chomp $line;
  $line =~ s/^\s*//;   $line =~ s/\s*$//;
  die "Strange line, \"$line\" found in ledger output" unless
    $line =~ /^\s*(\S+\:[^\$]+)\s+\$?\s*([\-\d\.\,]+)\s*$/;

  my($acct, $value) = ($1, $2);
  $acct =~ s/^\s*//;   $acct =~ s/\s*$//;

  $laterInternalBalances{$acct} = $value;

}
close(LATER_ACCT_DATA); die "error reading ledger output: $!" unless $? == 0;

foreach my $acct (sort keys %externalBalances) {
  if (not defined $internalBalancesPeriod{$acct}) {
    if (not defined $laterInternalBalances{$acct}
       and not defined $internalBalancesHistorical{$acct}) {
      print "$acct\n",
            "    EXISTS in external data, but does not appear in Ledger.\n";
      next;
    } else {
      $internalBalancesPeriod{$acct} = $ZERO;
    }
  }
  # if the account is an Asset or a Liability, then we want the historical
  # balance ending on the $endDate, which is stored in the %internalBalancesHistorical
  $internalBalancesPeriod{$acct} = $internalBalancesHistorical{$acct}
    if ($acct =~ /^(?:Assets?|Liabilit(?:ies|y))/);

  print "$acct\n",
        "     Ledger:         $internalBalancesPeriod{$acct}\n",
        "     External Report: $externalBalances{$acct}\n"
    if ($internalBalancesPeriod{$acct} != $externalBalances{$acct});

  delete $internalBalancesPeriod{$acct};
}

foreach my $acct (sort keys %internalBalancesPeriod) {
  print "$acct EXISTS in Ledger, but does not appear in external data.\n";
}
###############################################################################
#
# Local variables:
# compile-command: "perl -c external-account-totals-reconcile.plx"
# End:

