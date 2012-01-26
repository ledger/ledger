# quick doc-building makefile used by website
# requires: man2html, texinfo

docs: ledger.1.html ledger.html ledger.pdf ledger3.html ledger3.pdf

%.1.html: %.1
	-man2html $< | tail -n+3 >$@

%.html: %.texi
	-makeinfo --force --html --no-split -o $@ $<

%.pdf: %.texi
	-texi2pdf -b -q $<
	rm -f $*.aux $*.cp $*.fn $*.ky $*.log $*.pg $*.toc $*.tp $*.vr

clean:
	rm -rf ledger.1.html ledger.html ledger3.html ledger.pdf ledger3.pdf
