#!/usr/bin/env perl

$timeout = 60;

use Finance::Quote;
use POSIX qw(strftime localtime time);

$q = Finance::Quote->new;
$q->timeout($timeout);
$q->require_labels(qw/price/);

%quotes = $q->fetch("nasdaq", $ARGV[0]);
if ($quotes{$ARGV[0], "price"}) {
    print strftime('%Y/%m/%d %H:%M:%S', localtime(time()));
    print " ", $ARGV[0], " ";
    print "\$", $quotes{$ARGV[0], "price"}, "\n";
} else {
    exit 1;
}
