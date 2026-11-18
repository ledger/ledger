# - Try to find utfcpp (UTF-8 with C++ in a portable way)
# Once done, this will define
#
#  UTFCPP_FOUND        - utf8.h was found
#  UTFCPP_INCLUDE_DIR  - the include directory holding utf8.h
#
# utfcpp is header-only.  A system installation is preferred (hint it with
# UTFCPP_PATH); the copy vendored below lib/utfcpp/v4/source is the fallback.

set(UTFCPP_FOUND FALSE)

find_path(UTFCPP_INCLUDE_DIR
    NAMES utf8.h
    HINTS "${UTFCPP_PATH}"
    PATHS "${PROJECT_SOURCE_DIR}/lib/utfcpp/v4/source"
)

if (UTFCPP_INCLUDE_DIR)
  set(UTFCPP_FOUND TRUE)
endif()
