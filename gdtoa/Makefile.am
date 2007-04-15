lib_LTLIBRARIES = libgdtoa.la

libgdtoa_la_CPPFLAGS = -I$(includedir)
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
	mkdir -p $(includedir)
	$(CC) $(CFLAGS) -o $(prefix)/arithchk $< || \
	$(CC) -DNO_LONG_LONG $(CFLAGS) -o $(prefix)/arithchk $<
	$(prefix)/arithchk > $(includedir)/$@
	rm -f $(prefix)/arithchk

gd_qnan.h: qnan.c arith.h
	$(CC) $(CFLAGS) -o $(prefix)/qnan -I$(includedir) $<
	$(prefix)/qnan > $(includedir)/$@
	rm -f $(prefix)/qnan

libgdtoa_la_LDFLAGS = -release 1.0

pkginclude_HEADERS = gdtoa.h gdtoaimp.h

CLEANFILES = arithchk qnan
DISTCLEANFILES = arithchk arith.h qnan qnan.h
