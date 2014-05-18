include(ProcessorCount)
ProcessorCount(PROCESSORS)

if (NOT PROCESSORS EQUAL 0)
  math(EXPR JOBS "${PROCESSORS} * 2")
  set(CTEST_BUILD_FLAGS -j${JOBS})
endif()

get_target_property(LEDGER_LOCATION ledger LOCATION)

add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} ${CTEST_BUILD_FLAGS})

add_subdirectory(unit)

if (HAVE_BOOST_PYTHON)
  set(TEST_PYTHON_FLAGS "--python")
endif()

macro(add_ledger_harness_tests _class)
  if (PYTHONINTERP_FOUND)
    file(GLOB ${_class}_TESTS *.test)
    foreach(TestFile ${${_class}_TESTS})
      get_filename_component(TestFile_Name ${TestFile} NAME_WE)
      string(FIND ${TestFile_Name} "_py" TestFile_IsPythonTest)
      if ((TestFile_IsPythonTest EQUAL -1) OR HAVE_BOOST_PYTHON)
        add_test(${_class}Test_${TestFile_Name}
          ${PYTHON_EXECUTABLE} ${PROJECT_SOURCE_DIR}/test/RegressTests.py 
          ${LEDGER_LOCATION} ${PROJECT_SOURCE_DIR} 
          ${TestFile} ${TEST_PYTHON_FLAGS})
        set_target_properties(check 
          PROPERTIES DEPENDS ${_class}Test_${TestFile_Name})
      endif()
    endforeach()
  endif()
endmacro(add_ledger_harness_tests _class)

add_subdirectory(manual)
add_subdirectory(baseline)
add_subdirectory(regress)

# jww (2014-04-17): This is temporary until we find a fix.
#if (PYTHONINTERP_FOUND)
#  set(_class DocTests)
#  file(GLOB ${_class}_TESTS ${PROJECT_SOURCE_DIR}/doc/*.texi)
#  foreach(TestFile ${${_class}_TESTS})
#    get_filename_component(TestFile_Name ${TestFile} NAME_WE)
#    add_test(${_class}Test_${TestFile_Name}
#      ${PYTHON_EXECUTABLE} ${PROJECT_SOURCE_DIR}/test/DocTests.py
#      --ledger ${LEDGER_LOCATION} --file ${TestFile})
#    set_target_properties(check PROPERTIES DEPENDS ${_class}Test_${TestFile_Name})
#  endforeach()
#endif()

### CMakeLists.txt ends here
