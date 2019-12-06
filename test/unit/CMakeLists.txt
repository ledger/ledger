macro(add_ledger_test _name)
  target_link_libraries(${_name} libledger)
  add_test(Ledger${_name} ${PROJECT_BINARY_DIR}/${_name})
  set_tests_properties(Ledger${_name}
    PROPERTIES ENVIRONMENT "PYTHONUNBUFFERED=1 TZ=${Ledger_TEST_TIMEZONE}")
endmacro(add_ledger_test _name)

include_directories(${PROJECT_SOURCE_DIR}/src)

if (BUILD_LIBRARY)
  add_executable(UtilTests t_times.cc)
  if (HAVE_BOOST_PYTHON)
    target_link_libraries(UtilTests ${Python_LIBRARIES})
  endif()
  add_ledger_test(UtilTests)

  add_executable(MathTests t_amount.cc t_commodity.cc t_balance.cc t_expr.cc t_value.cc)
  set_source_files_properties(t_amount.cc t_value.cc PROPERTIES COMPILE_FLAGS "-Wno-unused-comparison")
  if (HAVE_BOOST_PYTHON)
    target_link_libraries(MathTests ${Python_LIBRARIES})
  endif()
  add_ledger_test(MathTests)
endif()
