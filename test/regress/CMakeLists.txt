add_ledger_harness_tests(Regress)

if(Python_EXECUTABLE)
  add_test(NAME RegressTest_3273
    COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/3273.py
      --ledger $<TARGET_FILE:ledger>)
  set_tests_properties(RegressTest_3273 PROPERTIES
    ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE}"
    TIMEOUT 120)
endif()

# A ledger/ package directory on PYTHONPATH must not make the embedded
# ledger module a package.  The script sets PYTHONPATH itself, which the
# .test harness cannot.
if(Python_EXECUTABLE AND HAVE_BOOST_PYTHON)
  add_test(NAME RegressTest_python-stray-ledger-package
    COMMAND ${Python_EXECUTABLE}
      ${CMAKE_CURRENT_SOURCE_DIR}/python-stray-ledger-package.py
      $<TARGET_FILE:ledger>)
  set_tests_properties(RegressTest_python-stray-ledger-package PROPERTIES
    ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE}")
endif()
