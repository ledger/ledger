#!/bin/sh

# This build script is for OS X Lion users who have compiled openmpi and
# clang-3.1 from MacPorts.  I build my own Boost instead of using MacPorts'
# Boost in order to get better debugging support, and to link with libc++.

export PATH=$PATH:/opt/local/lib/openmpi/bin

cat > ~/user-config.jam <<EOF
using intel : : "/opt/intel/bin/icc" : ;
EOF

# jww (2012-04-24): This is still linking against /usr/lib/libc++.1.dylib
# instead of /usr/local/lib/libc++.1.dylib
make CXX=icc LD=icc CC=icc AR=xiar OPTJ=-j20     \
     BOOST_TOOLSET=intel DIR_SUFFIX=intel                             \
    BOOST_DEFINES="-sINTEL_PATH=/opt/intel/bin -sINTEL_VERSION=12 -sICU_PATH=/usr/local cxxflags=\"-I/usr/local/include -I/opt/local/include\" linkflags=\"-L/usr/local/lib -L/opt/local/lib\""
