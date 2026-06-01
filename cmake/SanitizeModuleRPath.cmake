# SanitizeModuleRPath.cmake - remove empty RPATH entries from the Python module.
#
# Invoked from src/CMakeLists.txt as a POST_BUILD step:
#   cmake -DLEDGER_PYTHON_MODULE=<path> -P cmake/SanitizeModuleRPath.cmake
#
# Why this is needed (GitHub issue #3229, Debian bug #1138597):
#
# The ledger Python extension module is produced by copying the build-tree
# libledger shared library and is installed with install(PROGRAMS ...).  Because
# it is a plain file rather than an install(TARGETS ...) artifact, CMake never
# rewrites its RPATH at install time.  The build-tree RPATH therefore survives
# into the installed/packaged module -- and when CMAKE_INSTALL_RPATH is set (so
# CMake can relocate the binary at install time) CMake pads the build-tree RPATH
# with empty entries to reserve room for the longer install RPATH.  The result,
# on a system whose dependencies live in the default library directories, is a
# module whose DT_RUNPATH is a list of empty path elements ("::::::::").
#
# ELF dynamic linkers interpret an empty RPATH/RUNPATH element as the current
# working directory, so such a module searches CWD before the system library
# directories.  An attacker who can place a shared library in the working
# directory of a process that imports ledger can then have it loaded ahead of
# the genuine system libraries (CWE-426 / CWE-427).
#
# We remove *only* the empty elements, leaving any genuine directories intact.
# That is important on systems that install dependencies outside the default
# search path (e.g. Nix, Homebrew, or a custom --prefix), where the module
# legitimately needs those directories on its RPATH to find Boost, GMP, Python,
# etc.  Stripping the RPATH wholesale would break it there.

if(NOT DEFINED LEDGER_PYTHON_MODULE)
  message(FATAL_ERROR "LEDGER_PYTHON_MODULE must be defined")
endif()

if(NOT EXISTS "${LEDGER_PYTHON_MODULE}")
  return()
endif()

# Read the current RPATH/RUNPATH.  We only need to *read* it; the rewrite below
# uses CMake's own file(RPATH_*) commands, so no patchelf/chrpath is required.
find_program(_readelf NAMES readelf llvm-readelf)
find_program(_objdump NAMES objdump llvm-objdump)

set(_dyn "")
if(_readelf)
  execute_process(COMMAND "${_readelf}" -d "${LEDGER_PYTHON_MODULE}"
    OUTPUT_VARIABLE _dyn ERROR_QUIET)
elseif(_objdump)
  execute_process(COMMAND "${_objdump}" -x "${LEDGER_PYTHON_MODULE}"
    OUTPUT_VARIABLE _dyn ERROR_QUIET)
else()
  # Without a reader we cannot inspect the RPATH safely; leave the module alone.
  message(STATUS
    "SanitizeModuleRPath: no readelf/objdump found; skipping RPATH check")
  return()
endif()

# Find the RPATH/RUNPATH line and capture the bracketed (readelf) or trailing
# (objdump) path list.
set(_rpath "")
set(_found FALSE)
string(REPLACE "\n" ";" _lines "${_dyn}")
foreach(_line IN LISTS _lines)
  if(_line MATCHES "\\((RUNPATH|RPATH)\\).*\\[(.*)\\]")          # readelf
    set(_rpath "${CMAKE_MATCH_2}")
    set(_found TRUE)
    break()
  elseif(_line MATCHES "^[ \t]*(RUNPATH|RPATH)[ \t]+(.+)$")      # objdump
    string(STRIP "${CMAKE_MATCH_2}" _rpath)
    set(_found TRUE)
    break()
  endif()
endforeach()

if(NOT _found)
  return()                       # no RPATH/RUNPATH at all -- nothing to do
endif()

# Drop empty elements by collapsing runs of ':' and trimming leading/trailing
# ones.  Filesystem paths do not contain ':', so this is unambiguous.
set(_cleaned "${_rpath}")
string(REGEX REPLACE ":+" ":" _cleaned "${_cleaned}")
string(REGEX REPLACE "^:" "" _cleaned "${_cleaned}")
string(REGEX REPLACE ":$" "" _cleaned "${_cleaned}")

if(_cleaned STREQUAL "")
  # Every element was empty: remove the RPATH/RUNPATH entirely.
  file(RPATH_REMOVE FILE "${LEDGER_PYTHON_MODULE}")
elseif(NOT _cleaned STREQUAL _rpath)
  # Some empty elements were interleaved with real directories: keep the real
  # directories, drop the empties.  NEW_RPATH is shorter than OLD_RPATH, so the
  # in-place edit always fits.
  file(RPATH_CHANGE FILE "${LEDGER_PYTHON_MODULE}"
    OLD_RPATH "${_rpath}" NEW_RPATH "${_cleaned}")
endif()
