cmake_minimum_required (VERSION 3.5)
project(utfcpptests LANGUAGES CXX)
enable_testing()

add_library(${PROJECT_NAME} INTERFACE)

include_directories("${PROJECT_SOURCE_DIR}/../source")

add_executable(negative negative.cpp)
add_executable(cpp11 test_cpp11.cpp)
add_executable(cpp17 test_cpp17.cpp)
add_executable(cpp20 test_cpp20.cpp)
add_executable(apitests apitests.cpp)

add_executable(noexceptionstests noexceptionstests.cpp)



target_compile_options(${PROJECT_NAME} INTERFACE
  $<$<CXX_COMPILER_ID:MSVC>:/W4>
  $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-Wall -Wextra -Wpedantic -Wconversion>)

target_compile_options(noexceptionstests PUBLIC -fno-exceptions)

set_target_properties(negative apitests noexceptionstests
                      PROPERTIES
                      CXX_STANDARD 98
                      CXX_STANDARD_REQUIRED YES
                      CXX_EXTENSIONS NO)

set_target_properties(cpp11
                      PROPERTIES
                      CXX_STANDARD 11
                      CXX_STANDARD_REQUIRED YES
                      CXX_EXTENSIONS NO)

set_target_properties(cpp17
                      PROPERTIES
                      CXX_STANDARD 17
                      CXX_STANDARD_REQUIRED YES
                      CXX_EXTENSIONS NO)

set_target_properties(cpp20
                      PROPERTIES
                      CXX_STANDARD 20
                      CXX_STANDARD_REQUIRED YES
                      CXX_EXTENSIONS NO)


add_test(negative_test negative ${PROJECT_SOURCE_DIR}/test_data/utf8_invalid.txt)
add_test(cpp11_test cpp11)
add_test(cpp17_test cpp17)
add_test(cpp20_test cpp20)
add_test(api_test apitests)
add_test(noexceptions_test noexceptionstests)

