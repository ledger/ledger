# Uncomment these if you are on OS X and want to build universal libraries.
# This is only important if you intend to produce a Ledger binary for
# installation.

STOW_ROOT	= /usr/local/Cellar/boost
PRODUCTS	= $(HOME)/Products

GCC_VERSION	= 4.7
BOOST_VERSION	= 1_52_0

CC		= gcc-mp-$(GCC_VERSION)
ifeq ($(CC),clang)
CXX		= clang++
LD		= llvm-ld
DIR_SUFFIX	= clang
OPTJ		=
else
CXX		= g++-mp-$(GCC_VERSION)
LD		= gcc-mp-$(GCC_VERSION)
DIR_SUFFIX	= gcc$(subst .,,$(GCC_VERSION))
OPTJ		= #-j8
endif
CFLAGS		= $(CPPFLAGS) -g2 -ggdb
LDFLAGS		= -g2 -ggdb

BOOST_SOURCE	= boost-release
ifeq ($(GCC_VERSION),4.7)
BOOST_DEFINES   = define=_GLIBCXX__PTHREADS=1
else
BOOST_DEFINES   =
endif
ifeq ($(CC),clang)
BOOST_TOOLSET	= clang
else
BOOST_TOOLSET	= darwin
endif
BOOST_FLAGS	= toolset=$(BOOST_TOOLSET) --layout=versioned \
		  link=shared threading=single $(BOOST_DEFINES)
BOOST_DIR	= boost_$(BOOST_VERSION)-$(DIR_SUFFIX)
BOOST_STOW	= $(STOW_ROOT)/$(BOOST_VERSION)
BOOST_BUILD	= $(PRODUCTS)/$(BOOST_DIR)

all: boost-build

prepare-boost:
	perl -i -pe 's/local command = \[ common\.get-invocation-command darwin : g\+\+ : .*/local command = [ common.get-invocation-command darwin : g++ : $(CXX) ] ;/;' $(BOOST_SOURCE)/tools/build/v2/tools/darwin.jam
	perl -i -pe 's/flags darwin\.compile OPTIONS : -no-cpp-precomp -gdwarf-2 (-fexceptions )?;/flags darwin\.compile OPTIONS : -gdwarf-2 \1;/;' $(BOOST_SOURCE)/tools/build/v2/tools/darwin.jam

boost-build: prepare-boost
	(cd $(BOOST_SOURCE) && \
	 sh bootstrap.sh && \
	 ./b2 $(OPTJ) debug release --prefix=$(BOOST_STOW) \
	    --build-dir=$(BOOST_BUILD) $(BOOST_FLAGS) install)

clean:
	-rm -fr $(BOOST_STOW) $(BOOST_BUILD)
