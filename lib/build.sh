#!/bin/sh

# This build script is for OS X Lion users who have compiled openmpi and
# clang-3.1 from MacPorts.  I build my own Boost instead of using MacPorts'
# Boost in order to get better debugging support, and to link with libc++.

#export PATH=$PATH:/opt/local/lib/openmpi/bin

cat > ~/user-config.jam <<EOF
using clang-darwin : : "/usr/local/bin/clang++" : <cxxflags>-std=c++11 ;
EOF

# jww (2012-04-24): This is still linking against /usr/lib/libc++.1.dylib
# instead of /usr/local/lib/libc++.1.dylib
make CXX=clang++ LD=clang++ CC=clang OPTJ=-j20                                 \
     BOOST_TOOLSET=clang-darwin DIR_SUFFIX=clang31                             \
     BOOST_DEFINES="-sHAVE_ICONV=1 -sHAVE_ICU=1 -sICU_PATH=/usr/local/opt/icu4c cxxflags=\"-g -std=c++11 $* -nostdlibinc -isystem /usr/local/include -isystem /usr/local/include/c++/v1 -isystem /usr/include -stdlib=libc++\" linkflags=\"-g $* -L/usr/local/lib -L/usr/lib /usr/local/lib/libc++.dylib -stdlib=libc++\""
