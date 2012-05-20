include(ProcessorCount)
ProcessorCount(PROCESSORS)

if(NOT PROCESSORS EQUAL 0)
  set(CTEST_BUILD_FLAGS -j${PROCESSORS})
  set(ctest_test_args ${ctest_test_args} PARALLEL_LEVEL ${PROCESSORS})
endif()

get_target_property(LEDGER_LOCATION ledger LOCATION)

add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND})

add_subdirectory(unit)
add_subdirectory(regress)

### CMakeLists.txt ends here
