# Uncomment these if you are on OS X and want to build universal libraries.
# This is only important if you intend to produce a Ledger binary for
# installation.

STOW_ROOT = /usr/local/stow

BOOST_SOURCE  = boost
BOOST_VERSION = 1_40_0

icu-release:
	-(cd icu/source; make distclean)
	(cd icu/source; sh autogen.sh; \
	 ./configure CPPFLAGS="" \
		     CFLAGS="$(ARCH_CFLAGS)" \
		     LDFLAGS="$(ARCH_LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --enable-static \
		     --prefix=$(STOW_ROOT)/icu && \
	 make install)

icu-debug:
	-(cd icu/source; make distclean)
	(cd icu/source; sh autogen.sh; \
	 ./configure CPPFLAGS="-D_GLIBCXX_DEBUG=1" \
		     CFLAGS="-g $(ARCH_CFLAGS)" \
		     LDFLAGS="-g $(ARCH_LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --enable-static --enable-debug \
		     --prefix=$(STOW_ROOT)/icu-debug && \
	 make install)

icu-build: icu-release icu-debug

boost-release:
	(cd $(BOOST_SOURCE) && \
	bjam release --prefix=$(STOW_ROOT)/boost_$(BOOST_VERSION) \
	    --build-dir=$(HOME)/Products/boost_$(BOOST_VERSION) \
	    --toolset=darwin --build-type=complete --layout=versioned \
	    -sHAVE_ICU=1 -sICU_PATH=/usr/local/stow/icu install)

boost-debug:
	(cd $(BOOST_SOURCE) && \
	bjam debug --prefix=$(STOW_ROOT)/boost_$(BOOST_VERSION) \
	    --build-dir=$(HOME)/Products/boost_$(BOOST_VERSION) \
	    --toolset=darwin --build-type=complete --layout=versioned \
	    define=_GLIBCXX_DEBUG=1 \
	    -sHAVE_ICU=1 -sICU_PATH=/usr/local/stow/icu-debug install)

boost-build: boost-release boost-debug

cppunit-release:
	-(cd cppunit; make distclean)
	(cd cppunit; sh autogen.sh; \
	 ./configure CPPFLAGS="" \
		     CFLAGS="$(ARCH_CFLAGS)" \
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

all: boost-build cppunit-build
