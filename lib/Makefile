# Uncomment these if you are on OS X and want to build universal libraries.
# This is only important if you intend to produce a Ledger binary for
# installation.

#ARCH_CFLAGS = -g -arch i386 -arch ppc -isysroot /Developer/SDKs/MacOSX10.5.sdk
#ARCH_LDFLAGS = -g -arch i386 -arch ppc -Wl,-syslibroot,/Developer/SDKs/MacOSX10.5.sdk

STOW_ROOT = /usr/local/stow

HERE = $(shell pwd)

all: boost-build cppunit-build # gdtoa

boost-release:
	(cd boost && \
	bjam release -j3 --prefix=$(STOW_ROOT)/boost \
	    --build-dir=$(HERE)/boost/build --toolset=darwin \
	    architecture=combined install)

boost-debug:
	(cd boost && \
	bjam debug -j3 --prefix=$(STOW_ROOT)/boost \
	    --build-dir=$(HERE)/boost/build --toolset=darwin \
	    architecture=combined define=_GLIBCXX_DEBUG=1 install)

boost-build: boost-release boost-debug

cppunit-release:
	-(cd cppunit; make distclean)
	(cd cppunit; \
	 ./configure CFLAGS="$(ARCH_CFLAGS)" \
		   LDFLAGS="$(ARCH_LDFLAGS)" \
		--prefix=$(STOW_ROOT)/cppunit \
		--disable-dependency-tracking && \
	 make install)

cppunit-debug:
	-(cd cppunit; make distclean)
	(cd cppunit; \
	 ./configure CPPFLAGS="-D_GLIBCXX_DEBUG=1" \
		   CFLAGS="-g $(ARCH_CFLAGS)" \
		   LDFLAGS="-g $(ARCH_LDFLAGS)" \
		--prefix=$(STOW_ROOT)/cppunit-debug \
		--disable-dependency-tracking && \
	 make install)

cppunit-build: cppunit-release cppunit-debug

gdtoa-debug:
	(cd gdtoa; \
	 ./configure CFLAGS="-g $(ARCH_CFLAGS)" \
		   LDFLAGS="-g $(ARCH_LDFLAGS)" \
		--prefix=$(STOW_ROOT)/gdtoa \
		--disable-dependency-tracking && \
	 make install)

gdtoa-build: gdtoa-debug
