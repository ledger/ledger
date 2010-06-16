# Uncomment these if you are on OS X and want to build universal libraries.
# This is only important if you intend to produce a Ledger binary for
# installation.

STOW_ROOT	= /usr/local/stow
PRODUCTS	= $(HOME)/Products

CC		= gcc-mp-4.5
CXX		= g++-mp-4.5
LD		= gcc-mp-4.5
CXX_VERSION	= 45
CPPFLAGS	= -D_GLIBCXX_DEBUG=1 -D_GLIBCXX_FULLY_DYNAMIC_STRING=1
CFLAGS		= $(CPPFLAGS) -g
LDFLAGS		= -g

BOOST_VERSION	= 1_43_0
BOOST_SOURCE	= boost_$(BOOST_VERSION)
BOOST_TOOLSET	= darwin
BOOST_DEFINES	= define=_GLIBCXX_DEBUG=1 define=_GLIBCXX_FULLY_DYNAMIC_STRING=1
#BOOST_FLAGS	= --architecture=x86 --address_model=32_64
BOOST_FLAGS	= --toolset=$(BOOST_TOOLSET) \
		  --build-type=complete --layout=versioned \
		  $(BOOST_DEFINES)
BOOST_DIR	= boost_$(BOOST_VERSION)-gcc$(CXX_VERSION)
BOOST_STOW	= $(STOW_ROOT)/$(BOOST_DIR)
BOOST_BUILD	= $(PRODUCTS)/$(BOOST_DIR)
ICU_FLAGS	= -sHAVE_ICU=1 -sICU_PATH=$(STOW_ROOT)/icu
BOOST_ICU_DIR	= boost_$(BOOST_VERSION)-icu-gcc$(CXX_VERSION)
BOOST_ICU_STOW	= $(STOW_ROOT)/$(BOOST_ICU_DIR)
BOOST_ICU_BUILD = $(PRODUCTS)/$(BOOST_ICU_DIR)

all: boost-build cppunit-build #icu-build boost-icu-build

boost-build:
	(cd $(BOOST_SOURCE) && \
	bjam debug --prefix=$(BOOST_STOW) --build-dir=$(BOOST_BUILD) \
	    $(BOOST_FLAGS) install)

cppunit-build:
	-(cd cppunit; make distclean)
	(cd cppunit; sh autogen.sh; \
	 ./configure CPPFLAGS="$(CPPFLAGS)" \
		     CFLAGS="$(CFLAGS)" \
		     LDFLAGS="$(LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --prefix=$(STOW_ROOT)/cppunit-gcc$(CXX_VERSION) \
		     --disable-doxygen --disable-dot && \
	 make install)

#icu-build:
#	-(cd icu/source; make distclean)
#	(cd icu/source; sh autogen.sh; \
#	 ./configure CPPFLAGS="$(CPPFLAGS)" \
#		     CFLAGS="$(CFLAGS)" \
#		     LDFLAGS="$(LDFLAGS)" \
#		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
#		     --enable-static --enable-debug \
#		     --prefix=$(STOW_ROOT)/icu-gcc$(CXX_VERSION) && \
#	 make install)
#
#boost-icu-build:
#	(cd $(BOOST_SOURCE) && \
#	bjam debug --prefix=$(BOOST_ICU_STOW) --build-dir=$(BOOST_ICU_BUILD) \
#	    $(BOOST_FLAGS) $(ICU_FLAGS) install)

clean:
	-rm -fr $(BOOST_STOW) $(BOOST_BUILD)
	-rm -fr $(BOOST_ICU_STOW) $(BOOST_ICU_BUILD)
	-rm -fr $(STOW_ROOT)/cppunit-gcc$(CXX_VERSION)
	-rm -fr $(STOW_ROOT)/icu-gcc$(CXX_VERSION)
	-(cd cppunit; make distclean)
	-(cd icu/source; make distclean)

lib-clean:
	-(cd cppunit; make distclean)
	-(cd icu/source; make distclean)
