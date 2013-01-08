#!/usr/bin/perl

use strict;
use warnings;

use Math::BigFloat;
use Date::Manip;

Math::BigFloat->precision(-2);
my $ZERO =  Math::BigFloat->new("0.00");
my $ONE_HUNDRED =  Math::BigFloat->new("100.00");

my $VERBOSE = 1;
my $DEBUG = 0;

my $LEDGER_BIN = "/usr/local/bin/ledger";

######################################################################
sub SubSetSumSolver ($$$) {
  my($numberList, $totalSought, $extractNumber) = @_;

  my($P, $N) = (0, 0);
  my $size = scalar(@{$numberList});
  my %Q;
  my(@L) =
     map { { val => &$extractNumber($_), obj => $_ } } @{$numberList};

  print STDERR "  TotalSought:", $totalSought if $VERBOSE;
  print STDERR "  L in this iteration:\n     [" if $VERBOSE;

  foreach my $ee (@L) {
    if ($ee->{val} < 0) {
      $N += $ee->{val}
    } else {
      $P += $ee->{val};
    }
    print STDERR $ee->{val}, ", " if $VERBOSE;
  }
  print STDERR "]\n    P = $P, N = $N\n" if ($VERBOSE);

  for (my $ii = 0 ; $ii <= $size ; $ii++ ) {
    $Q{$ii}{0}{value} = 1;
    $Q{$ii}{0}{list} = [];
  }
  for (my $jj = $N; $jj <= $P ; $jj++) {
    $Q{0}{$jj}{value} = ($L[0]{val} == $jj);
    $Q{0}{$jj}{list} = $Q{0}{$jj}{value} ? [ $L[0]{obj} ] : [];
  }
  for (my $ii = 1; $ii <= $size ; $ii++ ) {
    for (my $jj = $N; $jj <= $P ; $jj++) {
      if ($Q{$ii-1}{$jj}{value}) {
        $Q{$ii}{$jj}{value} = 1;

        $Q{$ii}{$jj}{list} = [] unless defined $Q{$ii}{$jj}{list};
        push(@{$Q{$ii}{$jj}{list}}, @{$Q{$ii-1}{$jj}{list}});

      } elsif ($L[$ii]{val} == $jj) {
        $Q{$ii}{$jj}{value} = 1;

        $Q{$ii}{$jj}{list} = [] unless defined $Q{$ii}{$jj}{list};
        push(@{$Q{$ii}{$jj}{list}}, $jj);
      } elsif ($Q{$ii-1}{$jj - $L[$ii]{val}}{value}) {
        $Q{$ii}{$jj}{value} = 1;
        $Q{$ii}{$jj}{list} = [] unless defined $Q{$ii}{$jj}{list};
        push(@{$Q{$ii}{$jj}{list}}, $L[$ii]{obj}, @{$Q{$ii-1}{$jj - $L[$ii]{val}}{list}});
      } else {
        $Q{$ii}{$jj}{value} = 0;
        $Q{$ii}{$jj}{list} = [];
      }
    }
  }
  foreach (my $ii = 0; $ii <= $size; $ii++) {
    foreach (my $jj = $N; $jj <= $P; $jj++) {
      print "Q($ii, $jj) == $Q{$ii}{$jj}{value} with List of ", join(", ", @{$Q{$ii}{$jj}{list}}), "\n";
    }
  }
  return [ $Q{$size}{$totalSought}{value}, \@{$Q{$size}{$totalSought}{list}}];
}
######################################################################
sub Commify ($) {
    my $text = reverse $_[0];
    $text =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
    return scalar reverse $text;
}
######################################################################
sub ParseNumber($) {
  $_[0] =~ s/,//g;
  return Math::BigFloat->new($_[0]);
}
if (@ARGV < 4) {
  print STDERR "usage: $0 <ACCOUNT_REGEX> <END_DATE> <BANK_STATEMENT_BALANCE> <LEDGER_OPTIONS>\n";
  exit 1;
}
######################################################################
sub ConvertTwoDigitPrecisionToInteger ($) {
  return sprintf("%d", $_[0] * $ONE_HUNDRED);
}
######################################################################
sub ConvertTwoDigitPrecisionToIntegerInEntry ($) {
  return ConvertTwoDigitPrecisionToInteger($_[0]->{amount});
}
######################################################################
my($account, $endDate, $balanceSought, @mainLedgerOptions) = @ARGV;

$balanceSought = ParseNumber($balanceSought);

my $err;
my $earliestStartDate = DateCalc(ParseDate($endDate), ParseDateDelta("- 1 month"), \$err);

die "Date calculation error on $endDate" if ($err);

my $startDate = ParseDate($endDate);

my @solution;
while ($startDate ge $earliestStartDate) {
  print "START LOOP ITR: $startDate $earliestStartDate\n";
  $startDate = DateCalc(ParseDate($startDate), ParseDateDelta("- 1 day"), \$err);
  die "Date calculation error on $endDate" if ($err);

  my $formattedStartDate = UnixDate($startDate, "%Y/%m/%d");

  print STDERR "Testing $formattedStartDate through $endDate: \n" if $VERBOSE;

  my(@fullCommand) = ($LEDGER_BIN, @mainLedgerOptions, '-V', '-X', '$',
                      '-b', $formattedStartDate, '-e', $endDate,
                      '-F', '"%(date)","%C","%P","%t"\n',
                      'reg', "/$account/");

  open(FILE, "-|", @fullCommand)
    or die "unable to run command ledger command: @fullCommand: $!";

  my @entries;

  foreach my $line (<FILE>) {
    die "Unable to parse output line from: $line"
      unless $line =~ /^\s*"([^"]*)","([^"]*)","([^"]*)","([^"]*)"\s*$/;
    my($date, $checkNum, $payee, $amount) = ($1, $2, $3, $4);
    die "$amount is not a valid amount"
      unless $amount =~ s/\s*\$\s*([\-\d\.\,]+)\s*$/$1/;
    $amount = ParseNumber($amount);

    push(@entries, { date => $date, checkNum => $checkNum, amount => $amount });
  }
  close FILE;
  die "unable to properly run ledger command: @fullCommand: $!" unless ($? == 0);

  @solution = ();
  if (@entries == 1) {
    @solution = ( (abs($entries[0]->{amount}) == abs($balanceSought)), \@entries);
  } else {
    @solution = SubSetSumSolver(\@entries, ConvertTwoDigitPrecisionToInteger($balanceSought),
                                \&ConvertTwoDigitPrecisionToIntegerInEntry);
  }
  if ($VERBOSE) {
    use Data::Dumper;
    print STDERR "Solution for $formattedStartDate, $balanceSought: \n", Data::Dumper->Dump(\@solution);
  }
  print STDERR "Solution Found: Dying" if ($solution[0]) and $VERBOSE;
#  last if ($solution[0]);
}
print "DONE LOOP: $startDate $earliestStartDate\n";
if ($solution[0]) {
  print "FINAL SOLUTION: ";
  foreach my $ee (@{$solution[1]}) {
    print "$ee->date, $ee->payee, $ee->amount\n";
  }
}
###############################################################################
#
# Local variables:
# compile-command: "perl -c bank-reconcilation.plx"
# End:
