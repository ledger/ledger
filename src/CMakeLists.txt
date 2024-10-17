set(LEDGER_CLI_SOURCES
  global.cc
  main.cc)

set(LEDGER_SOURCES
  stats.cc
  generate.cc
  csv.cc
  convert.cc
  draft.cc
  emacs.cc
  ptree.cc
  print.cc
  output.cc
  precmd.cc
  chain.cc
  filters.cc
  report.cc
  select.cc
  session.cc
  option.cc
  lookup.cc
  compare.cc
  iterators.cc
  timelog.cc
  textual.cc
  temps.cc
  journal.cc
  account.cc
  xact.cc
  post.cc
  item.cc
  format.cc
  query.cc
  scope.cc
  expr.cc
  op.cc
  parser.cc
  token.cc
  value.cc
  balance.cc
  quotes.cc
  history.cc
  pool.cc
  annotate.cc
  commodity.cc
  amount.cc
  stream.cc
  mask.cc
  times.cc
  error.cc
  utils.cc
  wcwidth.cc
  sha512.cc)

if (HAVE_GPGME)
  list(APPEND LEDGER_SOURCES
    gpgme.cc)
endif()

if (HAVE_BOOST_PYTHON)
  list(APPEND LEDGER_SOURCES
    py_account.cc
    py_amount.cc
    py_balance.cc
    py_commodity.cc
    py_expr.cc
    py_format.cc
    py_item.cc
    py_journal.cc
    py_post.cc
    py_session.cc
    py_times.cc
    py_utils.cc
    py_value.cc
    py_xact.cc
    pyinterp.cc
    pyledger.cc)
endif()

set(LEDGER_INCLUDES
  account.h
  amount.h
  annotate.h
  balance.h
  chain.h
  commodity.h
  compare.h
  context.h
  convert.h
  csv.h
  draft.h
  emacs.h
  error.h
  expr.h
  exprbase.h
  filters.h
  flags.h
  format.h
  generate.h
  global.h
  gpgme.h
  history.h
  item.h
  iterators.h
  journal.h
  lookup.h
  mask.h
  op.h
  option.h
  output.h
  parser.h
  pool.h
  post.h
  precmd.h
  predicate.h
  print.h
  pstream.h
  ptree.h
  pyinterp.h
  pyutils.h
  query.h
  quotes.h
  report.h
  scope.h
  select.h
  session.h
  stats.h
  stream.h
  temps.h
  timelog.h
  times.h
  token.h
  unistring.h
  utils.h
  value.h
  xact.h
  ${PROJECT_BINARY_DIR}/ledger.hh
  ${PROJECT_BINARY_DIR}/system.hh)

# Windows provides no strptime(), so supply our own.
if (WIN32 OR CYGWIN)
  list(APPEND LEDGER_INCLUDES
    strptime.h)
  list(APPEND LEDGER_SOURCES
    strptime.cc)
endif()

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
  if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_compile_options(
      # -Weverything
      # -Wno-disabled-macro-expansion
      # -Wno-padded
      # -Wno-weak-vtables
      # -Wno-exit-time-destructors
      # -Wno-global-constructors
      # -Wno-switch-enum
      # -Wno-missing-prototypes
      # -Wno-missing-noreturn
      # -Wno-unused-parameter
      # -Wno-c++98-compat
      # -fno-limit-debug-info
      -Wno-\#pragma-messages
      -Wno-unused-local-typedef
      --system-header-prefix=include/boost/
      --system-header-prefix=boost/)

    macro(ADD_PCH_RULE _header_filename _src_list _other_srcs)
      set(_pch_filename "${_header_filename}.pch")

      set_source_files_properties(
        ${${_src_list}} PROPERTIES COMPILE_FLAGS "-include ${_header_filename}")
      if (_other_srcs)
        set_source_files_properties(
          ${_other_srcs} PROPERTIES COMPILE_FLAGS "-include ${_header_filename}")
      endif()
      list(APPEND ${_src_list} ${_pch_filename})

      set(_args ${CMAKE_CXX_FLAGS})
      list(APPEND _args ${CMAKE_CXX_FLAGS_DEBUG})
      if (BUILD_LIBRARY)
        list(APPEND _args ${CMAKE_SHARED_LIBRARY_CXX_FLAGS})
      endif()
      list(APPEND _args "-std=c++11")
      if (CYGWIN)
        list(APPEND _args "-U__STRICT_ANSI__")
      endif()
      list(APPEND _args "-x c++-header " ${_inc})
      list(APPEND _args -c ${_header_filename} -o ${_pch_filename})

      get_directory_property(DIRINC INCLUDE_DIRECTORIES)
      foreach(_inc ${DIRINC})
        list(APPEND _args "-isystem " ${_inc})
      endforeach(_inc ${DIRINC})

      separate_arguments(_args)

      add_custom_command(OUTPUT ${_pch_filename}
        COMMAND rm -f ${_pch_filename}
        COMMAND ${CMAKE_CXX_COMPILER} ${CMAKE_CXX_COMPILER_ARG1} ${_args}
        DEPENDS ${_header_filename})
    endmacro(ADD_PCH_RULE _header_filename _src_list _other_srcs)

  elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(GXX_WARNING_FLAGS
      -pedantic
      -Wall
      -Winvalid-pch
      -Wextra
      -Wcast-align
      -Wcast-qual
      -Wfloat-equal
      -Wmissing-field-initializers
      -Wno-endif-labels
      -Wno-overloaded-virtual
      -Wsign-compare
      -Wsign-promo
      -Wwrite-strings
      -Wno-unused-parameter
      -Wno-old-style-cast
      -Wno-deprecated
      -Wno-strict-aliasing)

    add_compile_options(${GXX_WARNING_FLAGS})

    macro(ADD_PCH_RULE _header_filename _src_list _other_srcs)
      set(_gch_filename "${_header_filename}.gch")

      set_source_files_properties(
        ${${_src_list}} PROPERTIES COMPILE_FLAGS "-Winvalid-pch")
      if (_other_srcs)
        set_source_files_properties(
          ${_other_srcs} PROPERTIES COMPILE_FLAGS "-Winvalid-pch")
      endif()
      list(APPEND ${_src_list} ${_gch_filename})

      set(_args ${CMAKE_CXX_FLAGS})
      list(APPEND _args ${CMAKE_CXX_FLAGS_DEBUG})
      if (BUILD_LIBRARY)
        list(APPEND _args ${CMAKE_SHARED_LIBRARY_CXX_FLAGS})
      endif()
      list(APPEND _args ${GXX_WARNING_FLAGS})
      list(APPEND _args "-std=c++11 ")
      if (CYGWIN)
        list(APPEND _args "-U__STRICT_ANSI__")
      endif()
      list(APPEND _args "-x c++-header " ${_inc})
      list(APPEND _args -c ${_header_filename} -o ${_gch_filename})

      get_directory_property(DIRINC INCLUDE_DIRECTORIES)
      foreach(_inc ${DIRINC})
        list(APPEND _args "-I" ${_inc})
      endforeach(_inc ${DIRINC})

      separate_arguments(_args)

      add_custom_command(OUTPUT ${_gch_filename}
        COMMAND rm -f ${_gch_filename}
        COMMAND ${CMAKE_CXX_COMPILER} ${CMAKE_CXX_COMPILER_ARG1} ${_args}
        DEPENDS ${_header_filename})
    endmacro(ADD_PCH_RULE _header_filename _src_list _other_srcs)

  else()
    macro(ADD_PCH_RULE _header_filename _src_list _other_srcs)
    endmacro(ADD_PCH_RULE _header_filename _src_list _other_srcs)
  endif()
else()
  macro(ADD_PCH_RULE _header_filename _src_list _other_srcs)
  endmacro(ADD_PCH_RULE _header_filename _src_list _other_srcs)
endif()

if(PRECOMPILE_SYSTEM_HH AND NOT (COMMAND target_precompile_headers))
  # enable fallback for CMake versions older than 3.16 without target_precompile_headers
  add_pch_rule(${PROJECT_BINARY_DIR}/system.hh LEDGER_SOURCES LEDGER_CLI_SOURCES)
endif()

include(GNUInstallDirs)

if (BUILD_LIBRARY)
  set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")
  add_library(libledger SHARED ${LEDGER_SOURCES})
  add_ledger_library_dependencies(libledger)
  set_target_properties(libledger PROPERTIES
    PREFIX ""
    INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}"
    VERSION ${Ledger_VERSION_MAJOR}
    SOVERSION ${Ledger_VERSION_MAJOR})
  set_source_files_properties(
    ${LEDGER_CLI_SOURCES} PROPERTIES COMPILE_FLAGS "-fPIC")

  add_executable(ledger ${LEDGER_CLI_SOURCES})
  target_link_libraries(ledger libledger)
  if (HAVE_GPGME)
    target_link_libraries(ledger Gpgmepp)
  endif()

  if (HAVE_BOOST_PYTHON)
    target_link_libraries(ledger ${Python_LIBRARIES})
  endif()

  install(TARGETS libledger DESTINATION ${CMAKE_INSTALL_LIBDIR})
  install(FILES ${LEDGER_INCLUDES}
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/ledger)
else()
  add_executable(ledger ${LEDGER_SOURCES} main.cc global.cc)
  add_ledger_library_dependencies(ledger)
endif()

if (PRECOMPILE_SYSTEM_HH AND (COMMAND target_precompile_headers) AND (CMAKE_BUILD_TYPE STREQUAL "Debug"))
  if (BUILD_LIBRARY)
    target_precompile_headers(libledger PRIVATE ${PROJECT_BINARY_DIR}/system.hh)
    target_precompile_headers(ledger REUSE_FROM libledger)
  else()
    target_precompile_headers(ledger PRIVATE ${PROJECT_BINARY_DIR}/system.hh)
  endif()
endif()

if (USE_PYTHON)
  if (Python_SITEARCH)
    if (WIN32 AND NOT CYGWIN)
      set(_ledger_python_module_name "ledger.pyd")
    elseif(CMAKE_SYSTEM_NAME STREQUAL Darwin)
      set(_ledger_python_module_name "ledger.so")
    else()
      set(_ledger_python_module_name "ledger${CMAKE_SHARED_LIBRARY_SUFFIX}")
    endif()

    # FIXME: symlink would be sufficient:
    # maybe using install(CODE "...") and
    # execute_process(COMMAND "${CMAKE_COMMAND}" -E create_symlink ...).
    # Windows will need a special case due to not supporting symlinks.
    add_custom_command(
      TARGET libledger POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
      $<TARGET_FILE:libledger> "${CMAKE_BINARY_DIR}/${_ledger_python_module_name}")
    install(
      FILES "${CMAKE_BINARY_DIR}/${_ledger_python_module_name}"
      DESTINATION ${Python_SITEARCH})
  else()
    message(WARNING "Python_SITEARCH not set. Will not install python module.")
  endif()
endif()

install(TARGETS ledger DESTINATION ${CMAKE_INSTALL_BINDIR})

### CMakeLists.txt ends here
