#!/bin/sh

# This build script is for OS X Lion users who have compiled openmpi and
# clang-3.1 from MacPorts.  I build my own Boost instead of using MacPorts'
# Boost in order to get better debugging support, and to link with libc++.

export PATH=$PATH:/opt/local/lib/openmpi/bin

cat > ~/user-config.jam <<EOF
using clang-darwin : : "/usr/local/bin/clang++" : <cxxflags>-std=c++11 <include>/usr/local/include ;
EOF

# jww (2012-04-24): This is still linking against /usr/lib/libc++.1.dylib
# instead of /usr/local/lib/libc++.1.dylib
make "$@" OPTJ=-j20     \
    BOOST_DEFINES="-sICU_PATH=/opt/local cxxflags=\"-I/opt/local/include\" linkflags=\"-L/opt/local/lib\""
