include(ProcessorCount)
ProcessorCount(PROCESSORS)

if (NOT PROCESSORS EQUAL 0)
  math(EXPR JOBS "${PROCESSORS} * 2")
  set(CTEST_BUILD_FLAGS -j${JOBS})
endif()

add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} ${CTEST_BUILD_FLAGS})

add_subdirectory(unit)

if (HAVE_BOOST_PYTHON)
  set(TEST_PYTHON_FLAGS "--python")
endif()

macro(add_ledger_harness_tests _class)
  if (Python_EXECUTABLE)
    file(GLOB ${_class}_TESTS *.test)
    foreach(TestFile ${${_class}_TESTS})
      get_filename_component(TestFile_Name ${TestFile} NAME_WE)
      string(FIND ${TestFile_Name} "_py" TestFile_IsPythonTest)
      if ((TestFile_IsPythonTest GREATER -1))
        get_filename_component(TestFile_FullName ${TestFile} NAME)
        string(FIND ${TestFile_FullName} "_py.test" TestFile_IsAnyPythonTest)
        string(FIND ${TestFile_FullName} "_py${Python_VERSION_MAJOR}.test" TestFile_IsThisPythonTest)
        if ((TestFile_IsAnyPythonTest EQUAL -1) AND (TestFile_IsThisPythonTest EQUAL -1))
          continue()
        endif()
      endif()
      if ((TestFile_IsPythonTest EQUAL -1) OR HAVE_BOOST_PYTHON)
        add_test(NAME ${_class}Test_${TestFile_Name}
          COMMAND ${Python_EXECUTABLE} ${PROJECT_SOURCE_DIR}/test/RegressTests.py
          $<TARGET_FILE:ledger> ${PROJECT_SOURCE_DIR}
          ${TestFile} ${TEST_PYTHON_FLAGS})
        set_tests_properties(${_class}Test_${TestFile_Name}
          PROPERTIES ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE}")
      endif()
    endforeach()
  endif()
endmacro(add_ledger_harness_tests _class)

add_subdirectory(manual)
add_subdirectory(baseline)
add_subdirectory(regress)

if (Python_EXECUTABLE)
  set(_class DocTests)
  file(GLOB ${_class}_TESTS ${PROJECT_SOURCE_DIR}/doc/*.texi)
  foreach(TestFile ${${_class}_TESTS})
    get_filename_component(TestFile_Name ${TestFile} NAME_WE)
    add_test(NAME ${_class}Test_${TestFile_Name}
      COMMAND ${Python_EXECUTABLE} ${PROJECT_SOURCE_DIR}/test/${_class}.py
      --ledger $<TARGET_FILE:ledger> --file ${TestFile})
    set_tests_properties(${_class}Test_${TestFile_Name}
      PROPERTIES ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE}")
  endforeach()

  # CheckManpage and CheckTexinfo are disabled, since they do not work
  # reliably yet
  list(APPEND CheckOptions CheckBaselineTests)
  if (BUILD_DEBUG)
    # CheckManpage and CheckTexinfo require ledger to have been
    # built with debug support
    list(APPEND CheckOptions CheckManpage CheckTexinfo)
  endif()
  foreach(_class ${CheckOptions})
    add_test(NAME ${_class}
      COMMAND ${Python_EXECUTABLE} ${PROJECT_SOURCE_DIR}/test/${_class}.py
      --ledger $<TARGET_FILE:ledger> --source ${PROJECT_SOURCE_DIR})
    set_tests_properties(${_class}
      PROPERTIES ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE}")
  endforeach()

  if (HAVE_BOOST_PYTHON)
    add_test(NAME demo
      COMMAND ${Python_EXECUTABLE} ${PROJECT_SOURCE_DIR}/python/demo.py
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
    set_tests_properties(demo
      PROPERTIES ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE};PYTHONPATH=.")
  endif()
endif()

### CMakeLists.txt ends here
