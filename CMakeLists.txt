set(Required_CMake_Version 3.16.2)
set(Required_Boost_Version 1.72)
set(Required_Gmp_Version 6.1.2)
set(Required_Mpfr_Version 4.0.2)
set(Required_Icu_Version 63)
set(Required_Python_Version 3.10)
set(Required_Gpgmepp_Version 1.13.1)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

cmake_minimum_required(VERSION ${Required_CMake_Version})

option(CMAKE_BUILD_TYPE "CMake Build type" "Release")
if(BUILD_DEBUG)
  message(DEPRECATION "BUILD_DEBUG is deprecated! Set CMAKE_BUILD_TYPE to Debug instead!")
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "CMake Build type" FORCE)
  unset(BUILD_DEBUG CACHE)
endif()

project(ledger)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/doc")
include(LedgerVersion)

# Point CMake at any custom modules we may ship
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

set(Ledger_TEST_TIMEZONE "America/Chicago")

enable_testing()

add_compile_definitions(BOOST_FILESYSTEM_NO_DEPRECATED)
set(CMAKE_CXX_STANDARD 17 CACHE STRING "C++ standard")
if(CYGWIN)
  add_compile_options(-U__STRICT_ANSI__)
endif()

########################################################################

option(USE_PYTHON "Build support for the Python scripting bridge" OFF)
option(USE_DOXYGEN "Build reference documentation using Doxygen" OFF)

option(DISABLE_ASSERTS "Build without any internal consistency checks" OFF)
option(BUILD_DEBUG "Build support for runtime debugging" OFF)
option(PRECOMPILE_SYSTEM_HH "Precompile system.hh" ON)

option(USE_GPGME "Build with support for encrypted journals" OFF)
option(USE_SANITIZERS "Enable AddressSanitizer and UndefinedBehaviorSanitizer" OFF)
option(BUILD_LIBRARY "Build and install Ledger as a library" ON)
option(BUILD_DOCS "Build and install documentation" OFF)
option(BUILD_WEB_DOCS "Build version of documentation suitable for viewing online" OFF)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(DEBUG_MODE 1)
else()
  set(DEBUG_MODE 0)
endif()

if(DISABLE_ASSERTS)
  set(NO_ASSERTS 1)
else()
  set(NO_ASSERTS 0)
endif()

if(CLANG_GCOV)
  set(PROFILE_LIBS profile_rt)
  set(CMAKE_REQUIRED_LIBRARIES ${PROFILE_LIBS})
endif()

if(USE_SANITIZERS)
  add_compile_options(-fsanitize=address,undefined -fno-omit-frame-pointer)
  add_link_options(-fsanitize=address,undefined)
endif()

########################################################################

if(USE_PYTHON)
  if(NOT BUILD_LIBRARY)
    message(ERROR "Building the python module requires BUILD_LIBRARY=ON.")
  endif()

  find_package(Python
    COMPONENTS Interpreter Development)
  if(PYTHON_FOUND AND ${Python_VERSION} VERSION_GREATER_EQUAL ${Required_Python_Version})
    set(BOOST_PYTHON "python${Python_VERSION_MAJOR}${Python_VERSION_MINOR}")
    set(HAVE_BOOST_PYTHON 1)
    include_directories(SYSTEM ${Python_INCLUDE_DIRS})
  else()
    set(HAVE_BOOST_PYTHON 0)
    message("Could not find a Python library to use with Boost.Python")
  endif()
else()
  set(HAVE_BOOST_PYTHON 0)
endif()

# Set BOOST_ROOT to help CMake to find the right Boost version
find_package(Boost ${Required_Boost_Version}
  REQUIRED date_time filesystem iostreams regex unit_test_framework
  ${BOOST_PYTHON} OPTIONAL_COMPONENTS nowide
  CONFIG)

# enable Boost::nowide library (for UTF8 command line args on Windows)
set(HAVE_BOOST_NOWIDE 0)
if(Boost_NOWIDE_FOUND)
  set(HAVE_BOOST_NOWIDE 1)
endif()

include_directories(SYSTEM ${Boost_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIRS})

# Crypto
if(USE_GPGME)
  find_package(Gpgmepp ${Required_Gpgmepp_Version} REQUIRED)
  set(HAVE_GPGME 1)
  include_directories(SYSTEM ${Gpgmepp_INCLUDE_DIRS})
  link_directories(${Gpgmepp_LIBRARY_DIRS})
else()
  set(HAVE_GPGME 0)
endif()

########################################################################

include(FindICU)
find_package(ICU ${Required_Icu_Version} OPTIONAL_COMPONENTS uc i18n)

include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckFunctionExists)
include(CheckCSourceCompiles)
include(CheckCXXSourceCompiles)
include(CheckCXXSourceRuns)
include(CMakePushCheckState)

check_function_exists(getpwuid HAVE_GETPWUID)
check_function_exists(getpwnam HAVE_GETPWNAM)
check_function_exists(ioctl HAVE_IOCTL)
check_function_exists(isatty HAVE_ISATTY)

check_c_source_compiles("
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int main() {
  int status, pfd[2];
  status = pipe(pfd);
  status = fork();
  if(status < 0) {
    ;
  } else if(status == 0) {
    char *arg0 = NULL;

    status = dup2(pfd[0], STDIN_FILENO);

    close(pfd[1]);
    close(pfd[0]);

    execlp(\"\", arg0, (char *)0);
    perror(\"execl\");
    exit(1);
  } else {
    close(pfd[0]);
  }
  return 0;
}" UNIX_PIPES_COMPILES)

if(UNIX_PIPES_COMPILES)
  set(HAVE_UNIX_PIPES 1)
else()
  set(HAVE_UNIX_PIPES 0)
endif()

cmake_push_check_state()

set(CMAKE_REQUIRED_INCLUDES ${CMAKE_INCLUDE_PATH} ${Boost_INCLUDE_DIRS} ${ICU_INCLUDE_DIRS})
set(CMAKE_REQUIRED_LIBRARIES Boost::regex ${ICU_LIBRARIES} ${PROFILE_LIBS})

check_cxx_source_runs("
#include <boost/regex/icu.hpp>

using namespace boost;

int main() {
  std::string text = \"Активы\";
  std::basic_string<boost::uint32_t> expr_str = \"Активы\";
  u32regex r = make_u32regex(\"активы\", regex::perl | regex::icase);
  return u32regex_search(text, r) ? 0 : 1;
}" BOOST_REGEX_UNICODE_RUNS)

if(BOOST_REGEX_UNICODE_RUNS)
  set(HAVE_BOOST_REGEX_UNICODE 1)
else()
  set(HAVE_BOOST_REGEX_UNICODE 0)
endif()

cmake_pop_check_state()

########################################################################

include_directories(${CMAKE_INCLUDE_PATH})

macro(find_opt_library_and_header _header_var _header _lib_var _lib _have_var)
  if(${_have_var})
    message("-- Looking for ${_header} in ${_lib}")
    find_path(${_header_var} ${_header})
    if(NOT ${_header_var})
      message("-- Looking for ${_header} in ${_lib} - not found")
      set(${_have_var} 0)
    else()
      find_library(${_lib_var} ${_lib})
      if(NOT ${_lib_var})
        message("-- Looking for ${_header} in ${_lib} - not found")
        set(${_have_var} 0)
      else()
        message("-- Looking for ${_header} in ${_lib} - found")
        include_directories(SYSTEM "${${_header_var}}")
        set(${_have_var} 1)
      endif()
    endif()
  else()
    set(${_have_var} 0)
  endif()
endmacro()

macro(find_req_library_and_header _header_var _header _lib_var _lib)
  find_path(${_header_var} ${_header})
  if(NOT ${_header_var})
    message(SEND_ERROR "Could not find ${_header} on your system")
  else()
    include_directories(SYSTEM "${${_header_var}}")
    find_library(${_lib_var} ${_lib})
    if(NOT ${_lib_var})
      message(SEND_ERROR "Could not find library ${_lib} on your system")
    endif()
  endif()
endmacro()

find_req_library_and_header(GMP_PATH gmp.h GMP_LIB gmp)
if(GMP_PATH AND EXISTS "${GMP_PATH}/gmp.h")
  file(STRINGS "${GMP_PATH}/gmp.h" gmp_header_str REGEX "^#define[\t ]+__GNU_MP_VERSION(_MINOR|_PATCHLEVEL)?[\t ]+[0-9]+")
  string(REGEX REPLACE ".*#define __GNU_MP_VERSION[\t ]+([0-9]+).*" "\\1" GMP_VERSION_MAJOR "${gmp_header_str}")
  string(REGEX REPLACE ".*#define __GNU_MP_VERSION_MINOR[\t ]+([0-9]+).*" "\\1" GMP_VERSION_MINOR "${gmp_header_str}")
  string(REGEX REPLACE ".*#define __GNU_MP_VERSION_PATCHLEVEL[\t ]+([0-9]+).*" "\\1" GMP_VERSION_PATCH "${gmp_header_str}")
  unset(gmp_header_str)
  set(GMP_VERSION_STRING "${GMP_VERSION_MAJOR}.${GMP_VERSION_MINOR}.${GMP_VERSION_PATCH}")
  if("${GMP_VERSION_STRING}" VERSION_LESS ${Required_Gmp_Version})
    message(FATAL_ERROR "Ledger requires GMP version ${Expected_Gmp_Version} or greater, but only GMP version ${GMP_VERSION_STRING} was found")
  endif()
endif()

find_req_library_and_header(MPFR_PATH mpfr.h MPFR_LIB mpfr)
if(MPFR_PATH AND EXISTS "${MPFR_PATH}/mpfr.h")
  file(STRINGS "${MPFR_PATH}/mpfr.h" mpfr_header_str REGEX "^#define[\t ]+MPFR_VERSION_(MAJOR|MINOR|PATCHLEVEL)[\t ]+[0-9]+")
  string(REGEX REPLACE ".*#define MPFR_VERSION_MAJOR[\t ]+([0-9]+).*" "\\1" MPFR_VERSION_MAJOR "${mpfr_header_str}")
  string(REGEX REPLACE ".*#define MPFR_VERSION_MINOR[\t ]+([0-9]+).*" "\\1" MPFR_VERSION_MINOR "${mpfr_header_str}")
  string(REGEX REPLACE ".*#define MPFR_VERSION_PATCHLEVEL[\t ]+([0-9]+).*" "\\1" MPFR_VERSION_PATCH "${mpfr_header_str}")
  unset(mpfr_header_str)
  set(MPFR_VERSION_STRING "${MPFR_VERSION_MAJOR}.${MPFR_VERSION_MINOR}.${MPFR_VERSION_PATCH}")
  if("${MPFR_VERSION_STRING}" VERSION_LESS ${Required_Mpfr_Version})
    message(FATAL_ERROR "Ledger requires MPFR version ${Expected_Mpfr_Version} or greater, but only MPFR version ${MPFR_VERSION_STRING} was found")
  endif()
endif()


check_library_exists(edit add_history "" HAVE_EDIT)
find_opt_library_and_header(EDIT_PATH editline/readline.h EDIT_LIB edit HAVE_EDIT)
if(NOT HAVE_EDIT)
check_library_exists(readline add_history "" HAVE_READLINE)
find_opt_library_and_header(READLINE_PATH readline/history.h READLINE_LIB readline HAVE_READLINE)
endif()


#find_package(Gettext)           # Used for running tests

#if(GETTEXT_FOUND)
#  set(HAVE_GETTEXT 1)
#else()
  set(HAVE_GETTEXT 0)
#endif()

#find_path(INTL_PATH libintl.h)
#find_library(INTL_LIB intl)
#include_directories(SYSTEM "${INTL_PATH}")

########################################################################

macro(add_ledger_library_dependencies _target)
  target_link_libraries(${_target} ${Boost_LIBRARIES})
  target_link_libraries(${_target} ${GMP_LIB})
  target_link_libraries(${_target} ${MPFR_LIB})
  target_link_libraries(${_target} ${PROFILE_LIBS})
  if(HAVE_EDIT)
    target_link_libraries(${_target} ${EDIT_LIB})
  endif()
  if(HAVE_READLINE)
    target_link_libraries(${_target} ${READLINE_LIB})
  endif()
  if(HAVE_GETTEXT)
    target_link_libraries(${_target} ${INTL_LIB})
  endif()
  if(HAVE_GPGME)
    target_link_libraries(${_target} Gpgmepp)
  endif()
  if(HAVE_BOOST_REGEX_UNICODE)
    target_link_libraries(${_target} ${ICU_LIBRARIES})
  endif()
  if(HAVE_BOOST_PYTHON)
    if(CMAKE_SYSTEM_NAME STREQUAL Darwin)
      # Don't link directly to a Python framework on macOS, to avoid segfaults
      # when the module is imported from a different interpreter
      set_target_properties(${_target} PROPERTIES LINK_FLAGS "-undefined dynamic_lookup")
    else()
      target_link_libraries(${_target} ${Python_LIBRARIES})
    endif()
  endif()
endmacro()

########################################################################

include(FindUtfcpp)
if(UTFCPP_FOUND)
  include_directories("${UTFCPP_INCLUDE_DIR}")
else()
  message(FATAL_ERROR "Missing required header file: utf8.h\n"
    "Define UTFCPP_PATH or install utfcpp locally into the source tree below lib/utfcpp/."
    )
endif()

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})

# add the binary tree to the search path for include files so that we will
# find system.hh
include_directories("${PROJECT_BINARY_DIR}")

configure_file(
  ${PROJECT_SOURCE_DIR}/src/system.hh.in
  ${PROJECT_BINARY_DIR}/system.hh)

set(Ledger_BUILD_TYPE "${CMAKE_BUILD_TYPE}")

configure_file(
  ${PROJECT_SOURCE_DIR}/src/ledger.hh.in
  ${PROJECT_BINARY_DIR}/ledger.hh)

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  set(CMAKE_INCLUDE_SYSTEM_FLAG_CXX "-isystem ")
endif()

add_subdirectory(src)
add_subdirectory(doc)
add_subdirectory(contrib)
add_subdirectory(test)

########################################################################

# clang-format targets for code formatting
find_program(CLANG_FORMAT clang-format)
if(CLANG_FORMAT)
  file(GLOB_RECURSE ALL_SOURCE_FILES
    ${CMAKE_SOURCE_DIR}/src/*.cc
    ${CMAKE_SOURCE_DIR}/src/*.h)

  add_custom_target(reformat
    COMMAND ${CLANG_FORMAT} -i ${ALL_SOURCE_FILES}
    COMMENT "Reformatting source files with clang-format"
    VERBATIM)

  add_custom_target(lint
    COMMAND ${CLANG_FORMAT} --dry-run --Werror ${ALL_SOURCE_FILES}
    COMMENT "Checking source file formatting with clang-format"
    VERBATIM)
endif()

########################################################################

# build a CPack driven installer package
include(InstallRequiredSystemLibraries)

set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/LICENSE.md")
set(CPACK_PACKAGE_VERSION_MAJOR "${Ledger_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${Ledger_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${Ledger_VERSION_PATCH}${Ledger_VERSION_PRERELEASE}")

set(CPACK_GENERATOR "TBZ2")
set(CPACK_SOURCE_GENERATOR "TBZ2")
set(CPACK_SOURCE_PACKAGE_FILE_NAME
  "${CMAKE_PROJECT_NAME}-${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_SOURCE_IGNORE_FILES "/.git*;/.dir-locals.el;~$;/doc/website/;/doc/wiki/;/lib/*.sh;/lib/Makefile;/tools/;${CPACK_SOURCE_IGNORE_FILES}")

include(CPack)

### CMakeLists.txt ends here
