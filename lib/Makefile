# Uncomment these if you are on OS X and want to build universal libraries.
# This is only important if you intend to produce a Ledger binary for
# installation.

STOW_ROOT = /usr/local/stow
PRODUCTS  = $(HOME)/Products

EXTRA_DEFINES = -D_GLIBCXX_FULLY_DYNAMIC_STRING=1

BOOST_SOURCE  = boost
BOOST_VERSION = 1_40_0
BOOST_TOOLSET = darwin
BOOST_DEFINES = define=_GLIBCXX_FULLY_DYNAMIC_STRING=1
BOOST_FLAGS   = --toolset=$(BOOST_TOOLSET) \
		--build-type=complete --layout=versioned \
		$(BOOST_DEFINES)
ICU_FLAGS     = -sHAVE_ICU=1 -sICU_PATH=$(STOW_ROOT)/icu 
ICU_DBG_FLAGS = -sHAVE_ICU=1 -sICU_PATH=$(STOW_ROOT)/icu-debug

icu-release:
	-(cd icu/source; make distclean)
	(cd icu/source; sh autogen.sh; \
	 ./configure CPPFLAGS="$(EXTRA_DEFINES)" \
		     CFLAGS="$(EXTRA_DEFINES) $(ARCH_CFLAGS)" \
		     LDFLAGS="$(ARCH_LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --enable-static \
		     --prefix=$(STOW_ROOT)/icu && \
	 make install)

icu-debug:
	-(cd icu/source; make distclean)
	(cd icu/source; sh autogen.sh; \
	 ./configure CPPFLAGS="-D_GLIBCXX_DEBUG=1 $(EXTRA_DEFINES)" \
		     CFLAGS="-g $(EXTRA_DEFINES) $(ARCH_CFLAGS)" \
		     LDFLAGS="-g $(ARCH_LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --enable-static --enable-debug \
		     --prefix=$(STOW_ROOT)/icu-debug && \
	 make install)

icu-build: icu-release icu-debug

boost-icu-release:
	(cd $(BOOST_SOURCE) && \
	bjam release --prefix=$(STOW_ROOT)/boost_$(BOOST_VERSION)-icu \
	    --build-dir=$(PRODUCTS)/boost_$(BOOST_VERSION)-icu \
	    $(BOOST_FLAGS) $(ICU_FLAGS) install)

boost-icu-debug:
	(cd $(BOOST_SOURCE) && \
	bjam debug --prefix=$(STOW_ROOT)/boost_$(BOOST_VERSION)-icu \
	    --build-dir=$(PRODUCTS)/boost_$(BOOST_VERSION)-icu \
	    $(BOOST_FLAGS) define=_GLIBCXX_DEBUG=1 \
	    $(ICU_DBG_FLAGS) install)

boost-icu-build: boost-icu-release boost-icu-debug

boost-release:
	(cd $(BOOST_SOURCE) && \
	bjam release --prefix=$(STOW_ROOT)/boost_$(BOOST_VERSION) \
	    --build-dir=$(PRODUCTS)/boost_$(BOOST_VERSION) \
	    $(BOOST_FLAGS) install)

boost-debug:
	(cd $(BOOST_SOURCE) && \
	bjam debug --prefix=$(STOW_ROOT)/boost_$(BOOST_VERSION) \
	    --build-dir=$(PRODUCTS)/boost_$(BOOST_VERSION) \
	    $(BOOST_FLAGS) define=_GLIBCXX_DEBUG=1 install)

boost-build: boost-release boost-debug

cppunit-release:
	-(cd cppunit; make distclean)
	(cd cppunit; sh autogen.sh; \
	 ./configure CPPFLAGS="$(EXTRA_DEFINES)" \
		     CFLAGS="$(EXTRA_DEFINES) $(ARCH_CFLAGS)" \
		     LDFLAGS="$(ARCH_LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --prefix=$(STOW_ROOT)/cppunit \
		     --disable-doxygen --disable-dot && \
	 make install)

cppunit-debug:
	-(cd cppunit; make distclean)
	(cd cppunit; sh autogen.sh; \
	 ./configure CPPFLAGS="-D_GLIBCXX_DEBUG=1 $(EXTRA_DEFINES)" \
		     CFLAGS="-g $(EXTRA_DEFINES) $(ARCH_CFLAGS)" \
		     LDFLAGS="-g $(ARCH_LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --prefix=$(STOW_ROOT)/cppunit-debug \
		     --disable-doxygen --disable-dot && \
	 make install)

cppunit-build: cppunit-release cppunit-debug

all: boost-build boost-icu-build cppunit-build
