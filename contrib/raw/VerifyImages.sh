#!/bin/sh

grep -h '; RECEIPT: ' \
	*.dat \
	*/*.dat \
	| sed 's,\W*; RECEIPT: ,,g' \
	| tr , '\n' \
	| sort -u \
	| while read X
do
	[ -f "$X" ] \
		&& echo OK $X \
		|| echo XX $X
done
