lib_LTLIBRARIES = libgdtoa.la

libgdtoa_la_LDFLAGS = -release 1.0
libgdtoa_la_CPPFLAGS = -I$(top_builddir)
libgdtoa_la_SOURCES = \
	dmisc.c dtoa.c g_Qfmt.c g__fmt.c g_ddfmt.c g_dfmt.c g_ffmt.c \
	g_xLfmt.c g_xfmt.c gdtoa.c gethex.c gmisc.c hd_init.c hexnan.c \
	misc.c smisc.c strtoIQ.c strtoId.c strtoIdd.c strtoIf.c strtoIg.c \
	strtoIx.c strtoIxL.c strtod.c strtodI.c strtodg.c strtof.c strtopQ.c \
	strtopd.c strtopdd.c strtopf.c strtopx.c strtopxL.c strtorQ.c \
	strtord.c strtordd.c strtorf.c strtorx.c strtorxL.c sum.c ulp.c \
	strtodnrp.c

EXTRA_libgdtoa_la_SOURCES = arithchk.c qnan.c

BUILT_SOURCES = arith.h gd_qnan.h
CLEANFILES    = arith.h gd_qnan.h arithchk qnan
EXTRA_DIST    = LICENSE

arith.h: arithchk.c
	$(CC) $(CFLAGS) -o $(top_builddir)/arithchk $< || \
	$(CC) -DNO_LONG_LONG $(CFLAGS) -o $(top_builddir)/arithchk $<
	$(top_builddir)/arithchk > $(top_builddir)/$@
	rm -f $(top_builddir)/arithchk

gd_qnan.h: qnan.c arith.h
	$(CC) $(CFLAGS) -o $(top_builddir)/qnan -I$(top_builddir) $<
	$(top_builddir)/qnan > $(top_builddir)/$@
	rm -f $(top_builddir)/qnan

pkginclude_HEADERS = gdtoa.h gdtoaimp.h
