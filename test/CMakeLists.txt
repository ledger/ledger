include(ProcessorCount)
processorcount(PROCESSORS)

find_package(Python COMPONENTS Interpreter) # Used for running tests
if(NOT Python_FOUND)
  message(WARNING "Could not find Python. Tests will not be run.")
endif()

if(NOT PROCESSORS EQUAL 0)
  math(EXPR JOBS "${PROCESSORS} * 2")
  set(CTEST_BUILD_FLAGS -j${JOBS})
endif()

add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} ${CTEST_BUILD_FLAGS})

add_subdirectory(unit)

if(HAVE_BOOST_PYTHON)
  set(TEST_PYTHON_FLAGS "--python")
endif()

macro(add_ledger_harness_tests _class)
  if(Python_EXECUTABLE)
    file(GLOB ${_class}_TESTS *.test)
    foreach(TestFile ${${_class}_TESTS})
      get_filename_component(TestFile_Name ${TestFile} NAME_WE)
      string(FIND ${TestFile} "_py.test" TestFile_IsPythonTest)
      if((TestFile_IsPythonTest EQUAL -1) OR HAVE_BOOST_PYTHON)
        add_test(NAME ${_class}Test_${TestFile_Name}
          COMMAND ${Python_EXECUTABLE} ${PROJECT_SOURCE_DIR}/test/RegressTests.py
          --ledger $<TARGET_FILE:ledger> --sourcepath ${PROJECT_SOURCE_DIR}
          ${TestFile} ${TEST_PYTHON_FLAGS})
        set_tests_properties(${_class}Test_${TestFile_Name}
          PROPERTIES ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE}")
      endif()
    endforeach()
  endif()
endmacro()

add_subdirectory(manual)
add_subdirectory(baseline)
add_subdirectory(regress)

if(Python_EXECUTABLE)
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

  # CheckEnvVars tests that LEDGER_* environment variables are honored
  # (requires running ledger without --args-only, so uses a separate script)
  add_test(NAME CheckEnvVars
    COMMAND ${Python_EXECUTABLE} ${PROJECT_SOURCE_DIR}/test/CheckEnvVars.py
    --ledger $<TARGET_FILE:ledger> --sourcepath ${PROJECT_SOURCE_DIR})
  set_tests_properties(CheckEnvVars
    PROPERTIES ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE}")

  # CheckManpage and CheckTexinfo are disabled, since they do not work
  # reliably yet
  list(APPEND CheckOptions CheckBaselineTests)
  if(BUILD_DEBUG)
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

  if(HAVE_BOOST_PYTHON)
    add_test(NAME demo
      COMMAND ${Python_EXECUTABLE} ${PROJECT_SOURCE_DIR}/python/demo.py
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
    set_tests_properties(demo
      PROPERTIES ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE};PYTHONPATH=.")

    # Regression test for GitHub issue #513: loading the ledger module from
    # an external Python interpreter must not re-initialize Python, and
    # reading a journal with a `python` directive must not emit any
    # RuntimeWarnings (or, on Python 3.12+, abort with a fatal error from
    # PyImport_AppendInittab).  This test must run via the external Python
    # interpreter -- not the in-tree test harness, which executes python
    # blocks under ledger's embedded interpreter where the bug does not
    # manifest.
    add_test(NAME RegressPyTest_513
      COMMAND ${Python_EXECUTABLE} ${PROJECT_SOURCE_DIR}/test/regress/513.py
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
    set_tests_properties(RegressPyTest_513
      PROPERTIES ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE};PYTHONPATH=.")

    # Regression test for GitHub issue #3229 (Debian #1138597): the ledger
    # Python extension module must not carry an RPATH/RUNPATH with empty
    # elements, which ELF dynamic linkers treat as the current working
    # directory and which therefore permit library-search-path hijacking.  The
    # check is ELF-specific, so it is skipped on macOS (Mach-O) and Windows
    # (PE), which use different mechanisms and are not affected.
    if(UNIX AND NOT APPLE)
      add_test(NAME RegressPyTest_3229
        COMMAND ${Python_EXECUTABLE} ${PROJECT_SOURCE_DIR}/test/regress/3229.py
        "${PROJECT_BINARY_DIR}/ledger${CMAKE_SHARED_LIBRARY_SUFFIX}")
    endif()
  endif()
endif()

### CMakeLists.txt ends here
