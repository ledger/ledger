lib_LTLIBRARIES = libgdtoa.la

libgdtoa_la_CPPFLAGS = -I$(srcdir)
libgdtoa_la_SOURCES = \
	dmisc.c dtoa.c g_Qfmt.c g__fmt.c g_ddfmt.c g_dfmt.c g_ffmt.c \
	g_xLfmt.c g_xfmt.c gdtoa.c gethex.c gmisc.c hd_init.c hexnan.c \
	misc.c smisc.c strtoIQ.c strtoId.c strtoIdd.c strtoIf.c strtoIg.c \
	strtoIx.c strtoIxL.c strtod.c strtodI.c strtodg.c strtof.c strtopQ.c \
	strtopd.c strtopdd.c strtopf.c strtopx.c strtopxL.c strtorQ.c \
	strtord.c strtordd.c strtorf.c strtorx.c strtorxL.c sum.c ulp.c

EXTRA_libgdtoa_la_SOURCES = arithchk.c qnan.c

$(libgdtoa_la_SOURCES): arith.h gd_qnan.h

arith.h: arithchk.c
	$(CC) $(CFLAGS) -o $(srcdir)/arithchk $< || \
	$(CC) -DNO_LONG_LONG $(CFLAGS) -o $(srcdir)/arithchk $<
	$(srcdir)/arithchk > $(srcdir)/$@
	rm -f $(srcdir)/arithchk

gd_qnan.h: qnan.c arith.h
	$(CC) $(CFLAGS) -o $(srcdir)/qnan -I$(srcdir) $<
	$(srcdir)/qnan > $(srcdir)/$@
	rm -f $(srcdir)/qnan

libgdtoa_la_LDFLAGS = -release 1.0

pkginclude_HEADERS = gdtoa.h gdtoaimp.h

CLEANFILES = arithchk qnan
DISTCLEANFILES = arithchk arith.h qnan qnan.h
