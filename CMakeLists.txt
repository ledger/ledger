cmake_minimum_required (VERSION 2.8.0)
project (utf8cpp)
include_directories ("${PROJECT_SOURCE_DIR}/source")

add_executable(smoke ${PROJECT_SOURCE_DIR}/test_drivers/smoke_test/test.cpp)
add_executable(negative ${PROJECT_SOURCE_DIR}/test_drivers/negative/negative.cpp)
add_executable(utf8reader ${PROJECT_SOURCE_DIR}/test_drivers/utf8reader/utf8reader.cpp)

add_executable(docsample ${PROJECT_SOURCE_DIR}/samples/docsample.cpp)

enable_testing()
add_test(smoke_test smoke)
add_test(negative_test negative ${PROJECT_SOURCE_DIR}/test_data/negative/utf8_invalid.txt)
