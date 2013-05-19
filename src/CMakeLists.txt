set(LEDGER_SOURCES
  stats.cc
  generate.cc
  csv.cc
  convert.cc
  draft.cc
  emacs.cc
  org.cc
  ptree.cc
  print.cc
  output.cc
  precmd.cc
  chain.cc
  filters.cc
  report.cc
  views.cc
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
  archive.cc
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
  utils.cc)

if(HAVE_BOOST_PYTHON)
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
  archive.h
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
  history.h
  item.h
  iterators.h
  journal.h
  lookup.h
  mask.h
  op.h
  option.h
  org.h
  output.h
  parser.h
  pool.h
  post.h
  precmd.h
  predicate.h
  print.h
  pstream.h
  ptree.h
  pyfstream.h
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
  views.h
  xact.h
  ${PROJECT_BINARY_DIR}/system.hh)

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  if((CMAKE_CXX_COMPILER MATCHES "clang") OR (CMAKE_CXX_COMPILER MATCHES "cxx"))
    add_definitions(
      -Weverything
      -Wno-disabled-macro-expansion
      -Wno-padded
      -Wno-weak-vtables
      -Wno-exit-time-destructors
      -Wno-global-constructors
      -Wno-switch-enum
      -Wno-missing-prototypes
      -Wno-missing-noreturn
      -Wno-unused-parameter
      -Wno-c++98-compat
      -fno-limit-debug-info)
  
    macro(ADD_PCH_RULE _header_filename _src_list _other_srcs)
      set(_pch_filename "${_header_filename}.pch")

      set_source_files_properties(
        ${${_src_list}} PROPERTIES COMPILE_FLAGS "-include ${_header_filename}")
      if(_other_srcs)
        set_source_files_properties(
          ${_other_srcs} PROPERTIES COMPILE_FLAGS "-include ${_header_filename}")
      endif()
      list(APPEND ${_src_list} ${_pch_filename})

      set(_args ${CMAKE_CXX_FLAGS})
      list(APPEND _args ${CMAKE_CXX_FLAGS_DEBUG})
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
  
  elseif(CMAKE_CXX_COMPILER MATCHES "g\\+\\+")
    set(GXX_WARNING_FLAGS
      -ansi
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

    add_definitions(${GXX_WARNING_FLAGS})
  
    macro(ADD_PCH_RULE _header_filename _src_list _other_srcs)
      set(_gch_filename "${_header_filename}.gch")

      set_source_files_properties(
        ${${_src_list}} PROPERTIES COMPILE_FLAGS "-Winvalid-pch")
      if(_other_srcs)
        set_source_files_properties(
          ${_other_srcs} PROPERTIES COMPILE_FLAGS "-Winvalid-pch")
      endif()
      list(APPEND ${_src_list} ${_gch_filename})

      set(_args ${CMAKE_CXX_FLAGS})
      list(APPEND _args ${CMAKE_CXX_FLAGS_DEBUG})
      list(APPEND _args ${GXX_WARNING_FLAGS})
      list(APPEND _args "-x c++-header " ${_inc})
      list(APPEND _args -c ${_header_filename} -o ${_gch_filename})

      get_directory_property(DIRINC INCLUDE_DIRECTORIES)
      foreach(_inc ${DIRINC})
        list(APPEND _args "-isystem " ${_inc})
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

add_pch_rule(${PROJECT_BINARY_DIR}/system.hh LEDGER_SOURCES main.cc global.cc)

if(BUILD_LIBRARY)
  add_library(libledger ${LEDGER_SOURCES} ${PROJECT_SOURCE_DIR}/lib/sha1.cpp)
  set_target_properties(libledger PROPERTIES OUTPUT_NAME ledger)

  add_executable(ledger main.cc global.cc)

  install(TARGETS libledger DESTINATION lib)
  install(FILES ${LEDGER_INCLUDES} DESTINATION include/ledger)
else()
  add_executable(ledger
    ${LEDGER_SOURCES} ${PROJECT_SOURCE_DIR}/lib/sha1.cpp main.cc global.cc)
endif()

add_ledger_library_dependencies(ledger)

install(TARGETS ledger DESTINATION bin)

### CMakeLists.txt ends here
