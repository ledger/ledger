file(READ ${PROJECT_SOURCE_DIR}/LICENSE.md Ledger_LICENSE)

configure_file(
  ${CMAKE_CURRENT_LIST_DIR}/__init__.py.in
  ${_ledger_python_module_dir}/__init__.py)

file(COPY
  ${PROJECT_SOURCE_DIR}/python/core
  ${PROJECT_SOURCE_DIR}/python/demo
  DESTINATION ${_ledger_python_module_dir})

install(DIRECTORY
  ${PROJECT_SOURCE_DIR}/python/core
  ${PROJECT_SOURCE_DIR}/python/demo
  DESTINATION "${Python_SITEARCH}/${PROJECT_NAME}")
