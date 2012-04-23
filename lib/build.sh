#!/bin/sh

# This build script is for OS X Lion users who have compiled openmpi and
# clang-3.1 from MacPorts.  I build my own Boost instead of using MacPorts'
# Boost in order to get better debugging support, and to link with libc++.

export PATH=$PATH:/opt/local/lib/openmpi/bin

cat > ~/user-config.jam <<EOF
using clang-darwin : : "/opt/local/bin/clang++-mp-3.1" : <cxxflags>-std=c++11 ;
EOF

make CXX=clang++-mp-3.1 LD=clang++-mp-3.1 CC=clang-mp-3.1 OPTJ=-j16     \
     CXXFLAGS="-g -std=c++11 -stdlib=libc++"                            \
     LDFLAGS="-g -stdlib=libc++"                                        \
     BOOST_TOOLSET=clang DIR_SUFFIX=clang31                             \
     BOOST_DEFINES="include=/opt/local/include -sICU_PATH=/opt/local -sICONV_PATH=/opt/local cxxflags=\"-g -std=c++11 -stdlib=libc++\" linkflags=\"-g -stdlib=libc++\""