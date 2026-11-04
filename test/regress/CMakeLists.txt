add_ledger_harness_tests(Regress)

if(Python_EXECUTABLE)
  add_test(NAME RegressTest_3273
    COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/3273.py
      --ledger $<TARGET_FILE:ledger>)
  set_tests_properties(RegressTest_3273 PROPERTIES
    ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE}"
    TIMEOUT 120)
endif()
