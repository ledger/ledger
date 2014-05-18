macro(add_ledger_test _name)
  target_link_libraries(${_name} libledger)
  add_test(Ledger${_name} ${PROJECT_BINARY_DIR}/${_name})
endmacro(add_ledger_test _name)

include_directories(${PROJECT_SOURCE_DIR}/src)

if (BUILD_LIBRARY)
  add_executable(UtilTests t_times.cc)
  add_ledger_test(UtilTests)

  add_executable(MathTests t_amount.cc t_commodity.cc t_balance.cc t_expr.cc)
  add_ledger_test(MathTests)

  set_target_properties(check PROPERTIES DEPENDS LedgerUtilTests)
  set_target_properties(check PROPERTIES DEPENDS LedgerMathTests)
endif()
