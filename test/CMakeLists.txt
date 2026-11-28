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

# libledger (and the Python module, a copy of it) must not export the
# OpenSSL-named C symbols of the vendored SHA-512 in src/sha512.cc.
if(BUILD_LIBRARY AND CMAKE_NM)
  add_test(NAME CheckHiddenSha512Symbols
    COMMAND ${CMAKE_COMMAND} -DNM=${CMAKE_NM} -DLIBRARY=$<TARGET_FILE:libledger>
    -P ${PROJECT_SOURCE_DIR}/test/CheckHiddenSha512Symbols.cmake)
endif()

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

# add_ledger_script_test(NAME <name> SCRIPT <file> [ARGS <arg>...]
#                        [WORKING_DIRECTORY <dir>] [ENVIRONMENT <var=value>...]
#                        [TIMEOUT <seconds>] [SKIP_RETURN_CODE <code>]
#                        [RUN_SERIAL])
#
# Registers a test that runs one Python script, for a check the .test format
# cannot express, under the suite's time zone like every other test.
function(add_ledger_script_test)
  cmake_parse_arguments(PARSE_ARGV 0 _test "RUN_SERIAL"
    "NAME;SCRIPT;WORKING_DIRECTORY;TIMEOUT;SKIP_RETURN_CODE" "ARGS;ENVIRONMENT")
  set(_wd)
  if(DEFINED _test_WORKING_DIRECTORY)
    set(_wd WORKING_DIRECTORY ${_test_WORKING_DIRECTORY})
  endif()
  add_test(NAME ${_test_NAME} ${_wd}
    COMMAND ${Python_EXECUTABLE} ${_test_SCRIPT} ${_test_ARGS})
  set(_env "TZ=${Ledger_TEST_TIMEZONE}" ${_test_ENVIRONMENT})
  set_tests_properties(${_test_NAME} PROPERTIES ENVIRONMENT "${_env}")
  foreach(_property TIMEOUT SKIP_RETURN_CODE)
    if(DEFINED _test_${_property})
      set_tests_properties(${_test_NAME} PROPERTIES
        ${_property} ${_test_${_property}})
    endif()
  endforeach()
  if(_test_RUN_SERIAL)
    set_tests_properties(${_test_NAME} PROPERTIES RUN_SERIAL TRUE)
  endif()
endfunction()

add_subdirectory(manual)
add_subdirectory(baseline)
add_subdirectory(regress)

# Semantic bisimulation (the bisimulation section of the
# ledger-semantics README): every positive .test journal in
# baseline/regress/manual is additionally replayed against the Lean
# oracle whenever the suite runs (`make check`, `make test`, `ctest`).
# One batched ctest test rather than per-test oracle invocations: the
# oracle loads once for the whole corpus.  The test registers only
# when LEDGER_LEAN_DIR names an oracle tree at configure time.  The
# Nix build points it at the oracle the flake pins, and a development
# build opts in with the same tree (`nix build .#oracle`).  A release
# build sets nothing, registers nothing, and owes its users no Lean
# toolchain.  A registered test still exits 77 and shows as SKIPPED
# when lake is missing at run time — visibly, never as a silent pass.
if(Python_EXECUTABLE)
  if(DEFINED ENV{LEDGER_LEAN_DIR} AND
     NOT "$ENV{LEDGER_LEAN_DIR}" STREQUAL "")
    # Bake the oracle location seen at configure time into the test,
    # so the oracle that runs is the one whose presence registered the
    # test, not whatever happens to be in the environment later.
    set(_bisim_env "LEDGER_LEAN_DIR=$ENV{LEDGER_LEAN_DIR}")
    if(DEFINED ENV{LEDGER_LEAN_REV} AND
       NOT "$ENV{LEDGER_LEAN_REV}" STREQUAL "")
      list(APPEND _bisim_env "LEDGER_LEAN_REV=$ENV{LEDGER_LEAN_REV}")
    endif()
    # --gross and --value turn on the harness's two opt-in comparisons
    # (test/semantic/check_all.py): the oracle supports both, so CI
    # compares gross totals and market-valued balances as well as the
    # net comparison.  The flags stay off in check_all.py itself, so a
    # developer running the script by hand keeps the net comparison
    # alone.
    add_ledger_script_test(NAME SemanticBisimulation
      SCRIPT ${PROJECT_SOURCE_DIR}/test/semantic/check_all.py
      ARGS --ledger $<TARGET_FILE:ledger> --source ${PROJECT_SOURCE_DIR}
        --gross --value
      ENVIRONMENT ${_bisim_env}
      SKIP_RETURN_CODE 77
      TIMEOUT 3600)
  else()
    message(STATUS
      "SemanticBisimulation: not registered (LEDGER_LEAN_DIR unset)")
  endif()

  # Valuation footing (test/semantic/footing.py): on each positive
  # .test journal that balances at cost in one commodity C, bal -V
  # --unrealized, where its rows are in one commodity, and bal -X C
  # --unrealized, where its rows are in C, must total zero, and their
  # unrealized rows must equal minus the matching bal -G total.  The
  # script bins every journal it cannot compare and records why.  It
  # compares C++ with C++ and needs no Lean oracle, so it registers
  # wherever Python is found, outside the condition above.
  add_ledger_script_test(NAME ValuationFooting
    SCRIPT ${PROJECT_SOURCE_DIR}/test/semantic/footing.py
    ARGS --ledger $<TARGET_FILE:ledger> --source ${PROJECT_SOURCE_DIR}
    TIMEOUT 7200)
endif()

# Build-tooling checks: the source-package ignore list (cmake/Packaging.cmake),
# the configure-time guards in CMakeLists.txt, and "acprep info".  They test
# the build scripts rather than ledger, so none of them runs the binary.
add_test(NAME CheckCPackSourceIgnore
  COMMAND ${CMAKE_COMMAND}
    -DCONFIG=${CMAKE_BINARY_DIR}/CPackSourceConfig.cmake
    -DSOURCE_DIR=${PROJECT_SOURCE_DIR}
    -P ${PROJECT_SOURCE_DIR}/test/CheckCPackSourceIgnore.cmake)
if(Python_EXECUTABLE)
  # Each configure-guard case reconfigures a scratch copy of this build.
  add_ledger_script_test(NAME CheckConfigureErrors
    SCRIPT ${PROJECT_SOURCE_DIR}/test/CheckConfigureErrors.py
    ARGS --cmake ${CMAKE_COMMAND} --source ${PROJECT_SOURCE_DIR}
      --cache ${CMAKE_BINARY_DIR}/CMakeCache.txt)
  add_ledger_script_test(NAME CheckAcprepInfo
    SCRIPT ${PROJECT_SOURCE_DIR}/test/CheckAcprepInfo.py
    ARGS ${PROJECT_SOURCE_DIR})
endif()

if(Python_EXECUTABLE)
  set(_class DocTests)
  file(GLOB ${_class}_TESTS ${PROJECT_SOURCE_DIR}/doc/*.texi)
  foreach(TestFile ${${_class}_TESTS})
    get_filename_component(TestFile_Name ${TestFile} NAME_WE)
    add_ledger_script_test(NAME ${_class}Test_${TestFile_Name}
      SCRIPT ${PROJECT_SOURCE_DIR}/test/${_class}.py
      ARGS --ledger $<TARGET_FILE:ledger> --file ${TestFile})
  endforeach()

  # GenerateTests round-trips "ledger generate" for seeds 1-20: bal of each
  # generated journal must equal bal of its print output.  The script's range
  # end is exclusive, so "1 21" runs seeds 1 through 20.
  add_ledger_script_test(NAME GenerateTests
    SCRIPT ${PROJECT_SOURCE_DIR}/test/GenerateTests.py
    ARGS --ledger $<TARGET_FILE:ledger> --sourcepath ${PROJECT_SOURCE_DIR}
      ${PROJECT_SOURCE_DIR}/test 1 21)

  # CheckEnvVars tests that LEDGER_* environment variables are honored
  # (requires running ledger without --args-only, so uses a separate script)
  add_ledger_script_test(NAME CheckEnvVars
    SCRIPT ${PROJECT_SOURCE_DIR}/test/CheckEnvVars.py
    ARGS --ledger $<TARGET_FILE:ledger> --sourcepath ${PROJECT_SOURCE_DIR})

  # HarnessSelfTest checks the rules of the test harnesses themselves,
  # which no .test file can check (see the sections of the script).
  add_ledger_script_test(NAME HarnessSelfTest
    SCRIPT ${PROJECT_SOURCE_DIR}/test/HarnessSelfTest.py
    ARGS --ledger $<TARGET_FILE:ledger> --sourcepath ${PROJECT_SOURCE_DIR}
    TIMEOUT 300)

  # The scripts that run ledger outside the .test harness give it the
  # harness's environment, and the option checks and snapshot.py share one
  # option list.  The stub ledgers are /bin/sh scripts.
  if(UNIX)
    add_ledger_script_test(NAME RegressPyTest_harness_environment
      SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/harness-environment.py
      ARGS $<TARGET_FILE:ledger> ${PROJECT_SOURCE_DIR})
  endif()

  # Regression test for GitHub issue #3273: a failed REPL report cannot
  # contaminate the next report.
  add_ledger_script_test(NAME RegressPyTest_3273
    SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/3273.py
    ARGS --ledger $<TARGET_FILE:ledger> TIMEOUT 120)

  # Regression test for GitHub issue #3294: --args-only reads no default price
  # database, and without HOME the default is ./.pricedb, not ./.ledgerrc.
  # Like CheckEnvVars it runs ledger itself, because it must control HOME and
  # the working directory, which the .test harness cannot.
  add_ledger_script_test(NAME RegressPyTest_3294
    SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/3294.py
    ARGS $<TARGET_FILE:ledger>)

  # An include through a directory that may be searched but not listed reads
  # the file, as -f does.  Like 3294.py it runs ledger itself, because it must
  # set directory permissions, which the .test harness cannot.
  add_ledger_script_test(NAME RegressPyTest_include-unlistable-dir
    SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/include-unlistable-dir.py
    ARGS $<TARGET_FILE:ledger>
    SKIP_RETURN_CODE 77)

  # Includes list a directory once per parse, not once per include: 500
  # includes in a directory of 5,000 files stay within a small factor of the
  # same transactions in one file.  A timing comparison, so it runs ledger
  # itself, and alone, so that other tests neither load the machine nor
  # modify build/test while it measures.
  add_ledger_script_test(NAME RegressPyTest_include-listing-speed
    SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/include-listing-speed.py
    ARGS $<TARGET_FILE:ledger>
    RUN_SERIAL
    TIMEOUT 900)

  # The per-parse listing cache must notice a file a `python` block writes
  # between two includes.  Skips (77) when ledger has no Python support.
  add_ledger_script_test(NAME RegressPyTest_include-after-generate
    SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/include-after-generate.py
    ARGS $<TARGET_FILE:ledger>
    SKIP_RETURN_CODE 77)

  # CheckManpage and CheckTexinfo are disabled, since they do not work
  # reliably yet
  # Every script in CheckOptions reads ledger's option list from
  # "ledger --debug option.names", which prints only in a Debug build; on any
  # other build the script exits 77 and ctest shows it as SKIPPED.
  list(APPEND CheckOptions CheckBaselineTests CheckCompletion)
  if(BUILD_DEBUG)
    # CheckManpage and CheckTexinfo require ledger to have been
    # built with debug support
    list(APPEND CheckOptions CheckManpage CheckTexinfo)
  endif()
  foreach(_class ${CheckOptions})
    add_ledger_script_test(NAME ${_class}
      SCRIPT ${PROJECT_SOURCE_DIR}/test/${_class}.py
      ARGS --ledger $<TARGET_FILE:ledger> --source ${PROJECT_SOURCE_DIR}
      SKIP_RETURN_CODE 77)
  endforeach()

  # The checks above, DocTests and CheckEnvVars must exit 0, 1 or 77 only: a
  # failure count would wrap modulo 256, and a ledger without debug support
  # must make the option checks skip, not pass.  The stub ledgers are /bin/sh
  # scripts.
  if(UNIX)
    add_ledger_script_test(NAME RegressPyTest_check_scripts_exit_status
      SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/check-scripts-exit-status.py
      ARGS ${PROJECT_SOURCE_DIR})
  endif()

  # Regression test for GitHub issue #3277: no other entry at the top of the
  # source tree may begin with "README" unless it also begins with "README.md",
  # so that "README<TAB>" still completes to README.md in a shell.
  add_ledger_script_test(NAME RegressPyTest_3277
    SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/3277.py
    ARGS ${PROJECT_SOURCE_DIR})

  # test/snapshot.py must capture only options ledger accepts, and two
  # captures by one binary must agree (the clock pinned, XML addresses masked).
  add_ledger_script_test(NAME RegressPyTest_snapshot_tool
    SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/snapshot_tool.py
    ARGS $<TARGET_FILE:ledger> ${PROJECT_SOURCE_DIR})

  # Every src/ header that an installed header includes must be installed too
  # (LEDGER_INCLUDES in src/CMakeLists.txt), or the installed tree cannot compile.
  add_ledger_script_test(NAME RegressPyTest_installed_headers
    SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/installed-headers.py
    ARGS ${PROJECT_SOURCE_DIR})

  # An encrypted journal must parse exactly like its plain copy; the test
  # encrypts fixtures with gpg under a throwaway GNUPGHOME and exits 77
  # (skipped) when gpg is not installed.
  if(HAVE_GPGME)
    add_ledger_script_test(NAME RegressPyTest_gpg_encrypted_journal
      SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/gpg-encrypted-journal.py
      ARGS $<TARGET_FILE:ledger>
      SKIP_RETURN_CODE 77
      TIMEOUT 120)
  endif()

  # CheckAcprep keeps acprep's help text, options and Ubuntu Boost packages
  # in line with the phases, flavors and Boost components that exist.
  add_ledger_script_test(NAME CheckAcprep
    SCRIPT ${PROJECT_SOURCE_DIR}/test/CheckAcprep.py
    ARGS ${PROJECT_SOURCE_DIR})

  if(HAVE_BOOST_PYTHON)
    add_ledger_script_test(NAME demo
      SCRIPT ${PROJECT_SOURCE_DIR}/python/demo.py
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
      ENVIRONMENT PYTHONPATH=.)

    # Regression test for GitHub issue #513: loading the ledger module from
    # an external Python interpreter must not re-initialize Python, and
    # reading a journal with a `python` directive must not emit any
    # RuntimeWarnings (or, on Python 3.12+, abort with a fatal error from
    # PyImport_AppendInittab).  This test must run via the external Python
    # interpreter -- not the in-tree test harness, which executes python
    # blocks under ledger's embedded interpreter where the bug does not
    # manifest.
    add_ledger_script_test(NAME RegressPyTest_513
      SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/513.py
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
      ENVIRONMENT PYTHONPATH=.)

    # A ledger/ package directory on PYTHONPATH must not make the embedded
    # ledger module a package.  The script sets PYTHONPATH itself, which the
    # .test harness cannot.
    add_ledger_script_test(NAME RegressPyTest_python-stray-ledger-package
      SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/python-stray-ledger-package.py
      ARGS $<TARGET_FILE:ledger>)

    # The unittest suites for the Python bindings, one interpreter per
    # test/python/*Test.py file.  The commodity pool outlives
    # close_journal_files() (#978), so suites that share an interpreter see
    # one another's commodities, aliases and learned display precisions.
    file(GLOB _py_suites ${PROJECT_SOURCE_DIR}/test/python/*Test.py)
    foreach(_py_suite ${_py_suites})
      get_filename_component(_py_suite_name ${_py_suite} NAME_WE)
      add_ledger_script_test(NAME PyUnitTest_${_py_suite_name}
        SCRIPT ${_py_suite}
        WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
        ENVIRONMENT PYTHONPATH=.)
    endforeach()

    # Regression test for GitHub issue #3229 (Debian #1138597): the ledger
    # Python extension module must not carry an RPATH/RUNPATH with empty
    # elements, which ELF dynamic linkers treat as the current working
    # directory and which therefore permit library-search-path hijacking.  The
    # check is ELF-specific, so it is skipped on macOS (Mach-O) and Windows
    # (PE), which use different mechanisms and are not affected.
    if(UNIX AND NOT APPLE)
      add_ledger_script_test(NAME RegressPyTest_3229
        SCRIPT ${PROJECT_SOURCE_DIR}/test/regress/3229.py
        ARGS "${PROJECT_BINARY_DIR}/ledger${CMAKE_SHARED_LIBRARY_SUFFIX}")
    endif()
  endif()
endif()

### CMakeLists.txt ends here
