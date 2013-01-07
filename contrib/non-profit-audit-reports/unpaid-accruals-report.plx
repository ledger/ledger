#!/usr/bin/perl
# unpaid-acccurals-report.plx                                    -*- Perl -*-

#  This report is designed to create what our accounts call a "Schedule of
#  accounts payable".  and "Schedule of accounts receivable".



# Copyright (C) 2013  Bradley M. Kuhn
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

my(@ledgerOptions) = (@mainLedgerOptions,
                      '-V', '-X', '$',  '-e', $endDate, '-F',
                      '\"%(tag("Invoice"))\",\"%A\",\"%(date)\",\"%(payee)\",\"%22.108t\"\n',
                      '--limit', 'tag("Invoice") !~ /^\s*$/', 'reg');

my @possibleTypes = ('Accrued:Loans Receivable', 'Accrued:Accounts Payable',
                     'Accrued:Accounts Receivable', 'Accrued:Expenses');

my %data;
foreach my $type (@possibleTypes) {
  open(LEDGER_FUNDS, "-|", $LEDGER_CMD, @ledgerOptions, "/^$type/")
    or die "Unable to run $LEDGER_CMD @ledgerOptions: $!";

  while (my $line = <LEDGER_FUNDS>) {
    next if $line =~ /"\<Adjustment\>"/;
    die "Unable to parse output line $line from @ledgerOptions"

      unless $line =~ /^\s*"([^"]+)","([^"]+)","([^"]+)","([^"]+)","\s*\$\s*([\-\d\.\,]+)"\s*$/;
    my($invoice, $account, $date, $payee, $amount) = ($1, $2, $3, $4, $5);
    $amount = ParseNumber($amount);

    push(@{$data{$type}{$invoice}{entries}}, { account => $account, date => $date, payee => $payee, amount => $amount});
    $data{$type}{$invoice}{total} = $ZERO unless defined $data{$type}{$invoice}{total};
    $data{$type}{$invoice}{total} += $amount;
  }
  close LEDGER_FUNDS;
  die "Failure on ledger command for $type: $!" unless ($? == 0);

}
foreach my $type (keys %data) {
  foreach my $invoice (keys %{$data{$type}}) {
    delete $data{$type}{$invoice} if abs($data{$type}{$invoice}{total}) <= $TWO_CENTS;
  }
}
foreach my $type (keys %data) {
  delete $data{$type} if scalar(keys %{$data{$type}}) == 0;
}
foreach my $type (keys %data) {
  print "\"SCHEDULE OF $type\"\n\"ENDING:\",\"$formattedEndDate\"\n\n",
    '"DATE","PAYEE","ACCOUNT","AMOUNT","INVOICE"', "\n";
  foreach my $invoice (keys %{$data{$type}}) {
    my $vals;
    foreach my $vals (@{$data{$type}{$invoice}{entries}}) {
      print "\"$vals->{date}\",\"$vals->{payee}\",\"$vals->{account}\",\"\$$vals->{amount}\",\"link:$invoice\"\n";
    }
  }
  print "pagebreak\n";
}
###############################################################################
#
# Local variables:
# compile-command: "perl -c unpaid-accruals-report.plx"
# End:

