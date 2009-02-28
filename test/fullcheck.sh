#!/bin/sh

VALGRIND=''
if [ -x /usr/bin/valgrind ]; then
  VALGRIND=valgrind
fi

export MallocGuardEdges=1
export MallocScribble=1
export MallocPreScribble=1
export MallocCheckHeapStart=100
export MallocCheckHeapEach=100
export DYLD_INSERT_LIBRARIES=/usr/lib/libgmalloc.dylib
export MALLOC_PROTECT_BEFORE=1
export MALLOC_FILL_SPACE=1
export MALLOC_STRICT_SIZE=1

exec $VALGRIND $@
