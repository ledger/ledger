add_executable(negative ${PROJECT_SOURCE_DIR}/tests/negative.cpp)
add_executable(cpp11 ${PROJECT_SOURCE_DIR}/tests/test_cpp11.cpp)
add_executable(cpp17 ${PROJECT_SOURCE_DIR}/tests/test_cpp17.cpp)
add_executable(apitests
    ${PROJECT_SOURCE_DIR}/tests/test_checked_api.cpp
    ${PROJECT_SOURCE_DIR}/tests/test_unchecked_api.cpp
    ${PROJECT_SOURCE_DIR}/tests/test_checked_iterator.cpp
    ${PROJECT_SOURCE_DIR}/tests/test_unchecked_iterator.cpp
)

add_executable(noexceptionstests
    ${PROJECT_SOURCE_DIR}/tests/test_unchecked_api.cpp
    ${PROJECT_SOURCE_DIR}/tests/test_unchecked_iterator.cpp
)

target_link_libraries(negative PRIVATE utf8::cpp)
target_link_libraries(cpp11 PRIVATE utf8::cpp)
target_link_libraries(cpp17 PRIVATE utf8::cpp)
target_link_libraries(apitests PRIVATE utf8::cpp)
target_link_libraries(noexceptionstests PRIVATE utf8::cpp)

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

add_test(negative_test negative ${PROJECT_SOURCE_DIR}/tests/test_data/utf8_invalid.txt)
add_test(cpp11_test cpp11)
add_test(cpp17_test cpp17)
add_test(api_test apitests)
add_test(noexceptions_test noexceptionstests)

