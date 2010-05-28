# Uncomment these if you are on OS X and want to build universal libraries.
# This is only important if you intend to produce a Ledger binary for
# installation.

STOW_ROOT     = /usr/local/stow
PRODUCTS      = $(HOME)/Products

CC            = gcc-mp-4.4
CXX           = g++-mp-4.4
LD            = gcc-mp-4.4
CPPFLAGS      = -D_GLIBCXX_DEBUG=1 -D_GLIBCXX_FULLY_DYNAMIC_STRING=1
CFLAGS        = $(CPPFLAGS) -g
LDFLAGS       = -g

BOOST_VERSION = 1_43_0
BOOST_SOURCE  = boost_$(BOOST_VERSION)
BOOST_TOOLSET = darwin
BOOST_DEFINES = define=_GLIBCXX_DEBUG=1 define=_GLIBCXX_FULLY_DYNAMIC_STRING=1
#BOOST_FLAGS  = --architecture=x86 --address_model=32_64
BOOST_FLAGS   = --toolset=$(BOOST_TOOLSET) \
		--build-type=complete --layout=versioned \
		$(BOOST_DEFINES)
ICU_FLAGS     = -sHAVE_ICU=1 -sICU_PATH=$(STOW_ROOT)/icu

all: boost-build cppunit-build icu-build boost-icu-build

boost-build:
	(cd $(BOOST_SOURCE) && \
	bjam debug --prefix=$(STOW_ROOT)/boost_$(BOOST_VERSION) \
	    --build-dir=$(PRODUCTS)/boost_$(BOOST_VERSION) \
	    $(BOOST_FLAGS) install)

cppunit-build:
	-(cd cppunit; make distclean)
	(cd cppunit; sh autogen.sh; \
	 ./configure CPPFLAGS="$(CPPFLAGS)" \
		     CFLAGS="$(CFLAGS)" \
		     LDFLAGS="$(LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --prefix=$(STOW_ROOT)/cppunit \
		     --disable-doxygen --disable-dot && \
	 make install)

icu-build:
	-(cd icu/source; make distclean)
	(cd icu/source; sh autogen.sh; \
	 ./configure CPPFLAGS="$(CPPFLAGS)" \
		     CFLAGS="$(CFLAGS)" \
		     LDFLAGS="$(LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --enable-static --enable-debug \
		     --prefix=$(STOW_ROOT)/icu && \
	 make install)

boost-icu-build:
	(cd $(BOOST_SOURCE) && \
	bjam debug --prefix=$(STOW_ROOT)/boost_$(BOOST_VERSION)-icu \
	    --build-dir=$(PRODUCTS)/boost_$(BOOST_VERSION)-icu \
	    $(BOOST_FLAGS) $(ICU_FLAGS) install)
