cmake_minimum_required(VERSION 3.16)
project(ledger_cpp_integration_tests)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find the ledger-core library
find_library(LEDGER_CORE_LIB
    NAMES ledger_core
    PATHS ${CMAKE_CURRENT_SOURCE_DIR}/../../target/debug
          ${CMAKE_CURRENT_SOURCE_DIR}/../../target/release
    NO_DEFAULT_PATH
)

if(NOT LEDGER_CORE_LIB)
    message(FATAL_ERROR "ledger-core library not found. Please build the Rust library first with: cargo build")
endif()

# Include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../include)

# Compiler flags for debug builds
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0 -Wall -Wextra -Werror")
set(CMAKE_CXX_FLAGS_RELEASE "-O2 -DNDEBUG")

# Enable AddressSanitizer for debug builds (optional)
option(ENABLE_ASAN "Enable AddressSanitizer" OFF)
if(ENABLE_ASAN AND CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=address -fsanitize=undefined")
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG} -fsanitize=address -fsanitize=undefined")
endif()

# Test executable
add_executable(cpp_integration_tests
    test_main.cpp
    test_basic_ffi.cpp
    test_memory_management.cpp
    test_callbacks.cpp
    test_error_handling.cpp
    test_raii_wrappers.cpp
    test_performance.cpp
)

# Example application
add_executable(ledger_ffi_example
    example_usage.cpp
)

# Link libraries
target_link_libraries(cpp_integration_tests ${LEDGER_CORE_LIB})
target_link_libraries(ledger_ffi_example ${LEDGER_CORE_LIB})

# For macOS, might need additional system libraries
if(APPLE)
    target_link_libraries(cpp_integration_tests "-framework Security" "-framework CoreFoundation")
    target_link_libraries(ledger_ffi_example "-framework Security" "-framework CoreFoundation")
endif()

# For Linux, might need dl and pthread
if(UNIX AND NOT APPLE)
    target_link_libraries(cpp_integration_tests dl pthread)
    target_link_libraries(ledger_ffi_example dl pthread)
endif()

# Define test data directory
target_compile_definitions(cpp_integration_tests 
    PRIVATE TEST_DATA_DIR="${CMAKE_CURRENT_SOURCE_DIR}/test_data"
)

# Custom targets for different test runs
add_custom_target(run_tests
    COMMAND ./cpp_integration_tests
    DEPENDS cpp_integration_tests
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Running C++ integration tests"
)

add_custom_target(run_valgrind
    COMMAND valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes ./cpp_integration_tests
    DEPENDS cpp_integration_tests
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Running tests with Valgrind"
)

# Build the Rust library first
add_custom_target(build_rust_lib
    COMMAND cargo build
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/../..
    COMMENT "Building Rust library"
)

add_dependencies(cpp_integration_tests build_rust_lib)
add_dependencies(ledger_ffi_example build_rust_lib)

# Installation (optional)
install(TARGETS cpp_integration_tests ledger_ffi_example RUNTIME DESTINATION bin)