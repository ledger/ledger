cmake_minimum_required (VERSION 3.0.2)
project (utf8cpp VERSION 3.2.2 LANGUAGES CXX)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    set(IS_ROOT_PROJECT ON)
else()
    set(IS_ROOT_PROJECT OFF)
endif()

option(UTF8_TESTS "Enable tests for UTF8-CPP" ${IS_ROOT_PROJECT})
option(UTF8_INSTALL "Enable installation for UTF8-CPP" ${IS_ROOT_PROJECT})
option(UTF8_SAMPLES "Enable building samples for UTF8-CPP" ${IS_ROOT_PROJECT})

add_library(utf8cpp INTERFACE)
target_include_directories(utf8cpp INTERFACE
    "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/source>"
    $<INSTALL_INTERFACE:include/utf8cpp>
)
add_library(utf8::cpp ALIAS utf8cpp)

if(UTF8_INSTALL)
    include(CMakePackageConfigHelpers)
    if(MSVC)
        set(DEF_INSTALL_CMAKE_DIR CMake)
    else()
        include(GNUInstallDirs) # define CMAKE_INSTALL_*
        set(DEF_INSTALL_CMAKE_DIR ${CMAKE_INSTALL_LIBDIR}/cmake/utf8cpp)
    endif()

    write_basic_package_version_file(
        ${CMAKE_CURRENT_BINARY_DIR}/utf8cppConfigVersion.cmake
        VERSION ${PROJECT_VERSION}
        COMPATIBILITY SameMajorVersion
    )

    configure_package_config_file(
        ${PROJECT_SOURCE_DIR}/utf8cppConfig.cmake.in
        ${CMAKE_CURRENT_BINARY_DIR}/utf8cppConfig.cmake
        INSTALL_DESTINATION ${DEF_INSTALL_CMAKE_DIR}
    )

    install(DIRECTORY source/ DESTINATION include/utf8cpp)
    install(TARGETS utf8cpp EXPORT utf8cppTargets)
    install(EXPORT utf8cppTargets DESTINATION ${DEF_INSTALL_CMAKE_DIR})
    install(
        FILES
        ${CMAKE_CURRENT_BINARY_DIR}/utf8cppConfig.cmake
        ${CMAKE_CURRENT_BINARY_DIR}/utf8cppConfigVersion.cmake
        DESTINATION
        ${DEF_INSTALL_CMAKE_DIR}
    )
endif()

if(UTF8_SAMPLES)
    add_executable(docsample ${PROJECT_SOURCE_DIR}/samples/docsample.cpp)
    target_link_libraries(docsample PRIVATE utf8::cpp)
endif()

if(UTF8_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()
