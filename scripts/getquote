#!/usr/bin/perl

$timeout = 60;

use Finance::Quote;

$q = Finance::Quote->new;
$q->timeout($timeout);
$q->require_labels(qw/price/);

%quotes = $q->fetch("nasdaq", $ARGV[0]);
if ($quotes{$ARGV[0], "price"}) {
    print "\$", $quotes{$ARGV[0], "price"}, "\n";
} else {
    exit 1;
}
