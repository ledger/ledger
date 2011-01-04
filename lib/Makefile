# Uncomment these if you are on OS X and want to build universal libraries.
# This is only important if you intend to produce a Ledger binary for
# installation.

STOW_ROOT	= /usr/local/stow
PRODUCTS	= $(HOME)/Products

CC		= gcc-mp-4.5
ifeq ($(CC),clang)
CXX		= clang++
LD		= llvm-ld
DIR_SUFFIX	= clang
OPTJ            =
else
CXX		= g++-mp-4.5
LD		= gcc-mp-4.5
DIR_SUFFIX	= gcc45
OPTJ            = #-j16
endif
CPPFLAGS	= -D_GLIBCXX_FULLY_DYNAMIC_STRING=1
ifneq ($(CC),clang)
CPPFLAGS       += -D_GLIBCXX_DEBUG=1
endif
CFLAGS		= $(CPPFLAGS) -g
LDFLAGS		= -g

BOOST_VERSION	= 1_45_0
BOOST_SOURCE	= boost-release
BOOST_DEFINES	= define=_GLIBCXX_FULLY_DYNAMIC_STRING=1
ifeq ($(CC),clang)
BOOST_TOOLSET	= clang
else
BOOST_TOOLSET	= darwin
BOOST_DEFINES	= define=_GLIBCXX_DEBUG=1
endif
#BOOST_FLAGS	= --architecture=x86 --address_model=32_64
BOOST_FLAGS	= --toolset=$(BOOST_TOOLSET) \
		  --build-type=complete --layout=versioned \
		  $(BOOST_DEFINES)
BOOST_DIR	= boost_$(BOOST_VERSION)-$(DIR_SUFFIX)
BOOST_STOW	= $(STOW_ROOT)/$(BOOST_DIR)
BOOST_BUILD	= $(PRODUCTS)/$(BOOST_DIR)
ICU_FLAGS	= -sHAVE_ICU=1 -sICU_PATH=$(STOW_ROOT)/icu
BOOST_ICU_DIR	= boost_$(BOOST_VERSION)-icu-$(DIR_SUFFIX)
BOOST_ICU_STOW	= $(STOW_ROOT)/$(BOOST_ICU_DIR)
BOOST_ICU_BUILD = $(PRODUCTS)/$(BOOST_ICU_DIR)

all: boost-build cppunit-build #icu-build boost-icu-build

prepare-boost:
	perl -i -pe 's/local command = \[ common\.get-invocation-command darwin : g\+\+ : \$$\(command\) \] ;/local command = [ common.get-invocation-command darwin : g++ : $(CXX) ] ;/;' $(BOOST_SOURCE)/tools/build/v2/tools/darwin.jam
	perl -i -pe 's/flags darwin\.compile OPTIONS : -no-cpp-precomp -gdwarf-2 ;/flags darwin\.compile OPTIONS : -gdwarf-2 ;/;' $(BOOST_SOURCE)/tools/build/v2/tools/darwin.jam

boost-build: prepare-boost
	(cd $(BOOST_SOURCE) && \
	 sh bootstrap.sh && \
	 ./bjam $(OPTJ) debug --prefix=$(BOOST_STOW) --build-dir=$(BOOST_BUILD) \
	    $(BOOST_FLAGS) install)

cppunit-build:
	-(cd cppunit; make distclean)
	(cd cppunit; sh autogen.sh; \
	 ./configure CPPFLAGS="$(CPPFLAGS)" \
		     CFLAGS="$(CFLAGS)" \
		     LDFLAGS="$(LDFLAGS)" \
		     CC="$(CC)" CXX="$(CXX)" LD="$(LD)" \
		     --prefix=$(STOW_ROOT)/cppunit-$(DIR_SUFFIX) \
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
#		     --prefix=$(STOW_ROOT)/icu-$(DIR_SUFFIX) && \
#	 make install)
#
#boost-icu-build:
#	(cd $(BOOST_SOURCE) && \
#	bjam debug --prefix=$(BOOST_ICU_STOW) --build-dir=$(BOOST_ICU_BUILD) \
#	    $(BOOST_FLAGS) $(ICU_FLAGS) install)

clean:
	-rm -fr $(BOOST_STOW) $(BOOST_BUILD)
	-rm -fr $(BOOST_ICU_STOW) $(BOOST_ICU_BUILD)
	-rm -fr $(STOW_ROOT)/cppunit-$(DIR_SUFFIX)
	-rm -fr $(STOW_ROOT)/icu-$(DIR_SUFFIX)
	-(cd cppunit; make distclean)
	-(cd icu/source; make distclean)

lib-clean:
	-(cd cppunit; make distclean)
	-(cd icu/source; make distclean)
