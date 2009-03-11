# Uncomment these if you are on OS X and want to build universal libraries.
# This is only important if you intend to produce a Ledger binary for
# installation.

#ARCH_CFLAGS = -g -arch i386 -arch ppc -isysroot /Developer/SDKs/MacOSX10.5.sdk
#ARCH_LDFLAGS = -g -arch i386 -arch ppc -Wl,-syslibroot,/Developer/SDKs/MacOSX10.5.sdk

STOW_ROOT = /usr/local/stow

BOOST_SOURCE  = boost
BOOST_VERSION = 1_38_0

# architecture=combined
boost-release:
	(cd $(BOOST_SOURCE) && \
	bjam release -j3 --prefix=$(STOW_ROOT)/boost_$(BOOST_VERSION) \
	    --build-dir=$(HOME)/Products/boost_$(BOOST_VERSION) \
	    --toolset=darwin --build-type=complete install)

boost-debug:
	(cd $(BOOST_SOURCE) && \
	bjam debug -j3 --prefix=$(STOW_ROOT)/boost_$(BOOST_VERSION) \
	    --build-dir=$(HOME)/Products/boost_$(BOOST_VERSION) \
	    --toolset=darwin --build-type=complete \
	    define=_GLIBCXX_DEBUG=1 install)

boost-build: boost-release boost-debug

cppunit-release:
	-(cd cppunit; make distclean)
	(cd cppunit; sh autogen.sh; \
	 ./configure CFLAGS="$(ARCH_CFLAGS)" \
		     LDFLAGS="$(ARCH_LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --prefix=$(STOW_ROOT)/cppunit && \
	 make install)

cppunit-debug:
	-(cd cppunit; make distclean)
	(cd cppunit; sh autogen.sh; \
	 ./configure CPPFLAGS="-D_GLIBCXX_DEBUG=1" \
		     CFLAGS="-g $(ARCH_CFLAGS)" \
		     LDFLAGS="-g $(ARCH_LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --prefix=$(STOW_ROOT)/cppunit-debug && \
	 make install)

cppunit-build: cppunit-release cppunit-debug

build-all: cppunit-build boost-build
