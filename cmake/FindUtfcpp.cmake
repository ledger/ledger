# - Try to find utfcpp
# Once done, this will define
#
#  UTFCPP_FOUND        - system has utfcpp's utf8.h
#  UTFCPP_PATH         - the utfcpp include directories

include(CheckCXXSourceCompiles)

set(UTFCPP_FOUND FALSE)

find_path(UTFCPP_INCLUDE_DIR
    NAMES utf8.h
    HINTS "${UTFCPP_PATH}"
    PATHS "${PROJECT_SOURCE_DIR}/lib/utfcpp/v4/source"
)

if (UTFCPP_INCLUDE_DIR)
  set(CMAKE_REQUIRED_INCLUDES "${UTFCPP_INCLUDE_DIR}")
  set(UTFCPP_FOUND TRUE)
endif()

check_cxx_source_compiles("
#include <string>
#include \"utf8.h\"

int main(int argc, char** argv) {
    std::string input = std::string(\"utfcpp\");
    const char * p = input.c_str();
    std::size_t len = input.length();
    utf8::is_valid(p, p + len);
}" HAVE_WORKING_UTFCPP)
