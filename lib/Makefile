# Uncomment these if you are on OS X and want to build universal libraries.
# This is only important if you intend to produce a Ledger binary for
# installation.

#ARCH_CFLAGS = -g -arch i386 -arch ppc -isysroot /Developer/SDKs/MacOSX10.5.sdk
#ARCH_LDFLAGS = -g -arch i386 -arch ppc -Wl,-syslibroot,/Developer/SDKs/MacOSX10.5.sdk

STOW_ROOT = /usr/local/stow

all: boost cppunit # gdtoa

boost:
	(cd boost; \
	bjam release --prefix=$(STOW_ROOT)/boost \
	    --build-dir=/tmp/boost --toolset=darwin \
	    architecture=combined install && \
	bjam debug --prefix=$(STOW_ROOT)/boost \
	    --build-dir=/tmp/boost --toolset=darwin \
	    architecture=combined define=_GLIBCXX_DEBUG=1 install)

cppunit:
	(cd cppunit; \
	 configure CFLAGS="$(ARCH_CFLAGS)" \
		   LDFLAGS="$(ARCH_LDFLAGS)" \
		--prefix=$(STOW_ROOT)/cppunit \
		--disable-dependency-tracking && \
	 make install)
	(cd cppunit; \
	 configure CPPFLAGS="-D_GLIBCXX_DEBUG=1" \
		   CFLAGS="-g $(ARCH_CFLAGS)" \
		   LDFLAGS="-g $(ARCH_LDFLAGS)" \
		--prefix=$(STOW_ROOT)/cppunit-debug \
		--disable-dependency-tracking && \
	 make install)

gdtoa:
	(cd gdtoa; \
	 configure CFLAGS="$(ARCH_CFLAGS)" \
		   LDFLAGS="$(ARCH_LDFLAGS)" \
		--prefix=$(STOW_ROOT)/gdtoa \
		--disable-dependency-tracking && \
	 make install)
