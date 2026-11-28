# add_ledger_test(NAME SOURCE...) builds the unit-test executable NAME and
# registers it as the ctest LedgerNAME.  Unit tests use BOOST_TEST_DYN_LINK
# and need a direct link to the Boost.Test shared library so the linker
# can resolve unit_test_main.
function(add_ledger_test _name)
  add_executable(${_name} ${ARGN})
  target_link_libraries(${_name} libledger Boost::unit_test_framework)
  if(HAVE_BOOST_PYTHON)
    target_link_libraries(${_name} ${Python_LIBRARIES})
  endif()
  add_test(Ledger${_name} ${PROJECT_BINARY_DIR}/${_name})
  set_tests_properties(Ledger${_name}
    PROPERTIES ENVIRONMENT "TZ=${Ledger_TEST_TIMEZONE}")
endfunction()

include_directories(${PROJECT_SOURCE_DIR}/src)

# The top level finds Boost.Test as an optional component.
if(BUILD_LIBRARY AND TARGET Boost::unit_test_framework)
  add_ledger_test(UtilTests t_times.cc t_format.cc t_utils.cc)

  set_source_files_properties(t_amount.cc t_value.cc
    PROPERTIES COMPILE_FLAGS "-Wno-unused-comparison")
  add_ledger_test(MathTests t_amount.cc t_commodity.cc t_balance.cc t_expr.cc
    t_value.cc t_account.cc t_xact.cc)

  add_ledger_test(FilterTests t_filters.cc t_chain.cc)

  if(HAVE_GPGME)
    add_ledger_test(GpgmeTests t_gpgme.cc)
  endif()

  add_ledger_test(TextualTests t_textual.cc)
elseif(BUILD_LIBRARY)
  message(STATUS "Boost.Test (unit_test_framework) not found: unit tests are not built")
endif()
