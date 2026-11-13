# Check the source-package ignore list that cmake/Packaging.cmake sets in
# CPACK_SOURCE_IGNORE_FILES: "make package_source" must keep or drop each
# path below exactly as listed.
#
#   cmake -DCONFIG=<build>/CPackSourceConfig.cmake -DSOURCE_DIR=<source>
#         -P test/CheckCPackSourceIgnore.cmake

set(cases
  # Kept: sources, vendored libraries, and names that only resemble a pattern.
  "src/journal.cc|FALSE"
  "lib/immer/immer/map.hpp|FALSE"
  "lib/utfcpp/v4/source/utf8.h|FALSE"
  "doc/ledger3.texi|FALSE"
  "doc/agile/notes.txt|FALSE"
  "lib/refresh|FALSE"
  "test/regress/build-order.test|FALSE"
  "contrib/tools/README|FALSE"
  # Dropped: version-control files at any depth, editor files, unpublished
  # documentation, autotools-era helper scripts, and in-tree build trees.
  ".git/HEAD|TRUE"
  ".gitignore|TRUE"
  ".github/workflows/cmake.yml|TRUE"
  "lib/utfcpp/v4/.gitmodules|TRUE"
  ".dir-locals.el|TRUE"
  "src/journal.cc~|TRUE"
  "doc/website/index.html|TRUE"
  "doc/wiki/Home.md|TRUE"
  "lib/build.sh|TRUE"
  "lib/build-gcc.sh|TRUE"
  "lib/Makefile|TRUE"
  "tools/build.sh|TRUE"
  "build/CMakeCache.txt|TRUE"
  "build-opt/src/ledger|TRUE")

# CPack drops a file when any entry, as a regular expression, matches
# somewhere in the file's absolute path.
function(check_cases root regexes)
  foreach(case IN LISTS cases)
    string(REPLACE "|" ";" case "${case}")
    list(GET case 0 relpath)
    list(GET case 1 expected)
    set(ignored FALSE)
    foreach(regex IN LISTS regexes)
      if(NOT regex STREQUAL "" AND "${root}/${relpath}" MATCHES "${regex}")
        set(ignored TRUE)
      endif()
    endforeach()
    if(NOT ignored STREQUAL expected)
      message(SEND_ERROR
        "${root}/${relpath}: ignored is ${ignored}, expected ${expected}")
    endif()
  endforeach()
endfunction()

# The list as CPack reads it back from the generated configuration.
include("${CONFIG}")
check_cases("${SOURCE_DIR}" "${CPACK_IGNORE_FILES}")

# The list Packaging.cmake builds for a source root below directories named
# build and tools, whose own name holds regular expression metacharacters
# that must match only themselves.
unset(CPACK_SOURCE_IGNORE_FILES)
set(CMAKE_SOURCE_DIR "/build/tools/ledger+1.0 (copy)")
include("${SOURCE_DIR}/cmake/Packaging.cmake")
check_cases("${CMAKE_SOURCE_DIR}" "${CPACK_SOURCE_IGNORE_FILES}")
