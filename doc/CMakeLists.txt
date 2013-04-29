# The following will be generated or updated when the 'doc' target is built:
# • user guide and man pages: if BUILD_DOCS is set
# • HTML versions of the above: if BUILD_DOCS and BUILD_WEB_DOCS are set
# • Doxygen / reference documentation: if USE_DOXYGEN is set

########################################################################

if (USE_DOXYGEN)
  find_package(Doxygen)
  if(NOT DOXYGEN_FOUND)
    message(FATAL_ERROR "Could not find doxygen. Reference documentation cannot be built.")
  endif()

  configure_file(Doxyfile.in Doxyfile @ONLY)

  # see INPUT/FILE_PATTERNS in Doxyfile.in
  file(GLOB doxygen_input_files ${CMAKE_SOURCE_DIR}/src/*.h)

  add_custom_command(OUTPUT html/index.html
    COMMAND ${DOXYGEN_EXECUTABLE} Doxyfile
    DEPENDS Doxyfile ${doxygen_input_files}
    COMMENT "Building doxygen documentation")
  add_custom_target(doc.doxygen DEPENDS html/index.html)
else()
  add_custom_target(doc.doxygen)
endif()

########################################################################

if(NOT BUILD_DOCS)
  add_custom_target(doc DEPENDS doc.doxygen)
  return()
endif()

set(info_files ledger.texi ledger3.texi ledger-mode.texi)

find_program(MAKEINFO makeinfo)
find_program(TEXI2PDF texi2pdf)
find_program(MAN2HTML man2html)

########################################################################

foreach(file ${info_files})
  get_filename_component(file_base ${file} NAME_WE)
  if(BUILD_WEB_DOCS)
    if(NOT MAKEINFO)
      message(FATAL_ERROR "Could not find makeinfo. HTML version of documentation cannot be built.")
    endif()

    add_custom_command(OUTPUT ${file_base}.html
      COMMAND makeinfo --force --html --no-split -o ${file_base}.html ${CMAKE_CURRENT_SOURCE_DIR}/${file}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
      VERBATIM)
    list(APPEND ledger_doc_files ${file_base}.html)
  endif(BUILD_WEB_DOCS)

  if(NOT TEXI2PDF)
    message(WARNING "Could not find texi2pdf. PDF version of documentation will not be built.")
  else()
    add_custom_command(OUTPUT ${file_base}.pdf
      COMMAND texi2pdf -b -q -o ${file_base}.pdf ${CMAKE_CURRENT_SOURCE_DIR}/${file}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
      VERBATIM)
    list(APPEND ledger_doc_files ${file_base}.pdf)
  endif()
endforeach()

########################################################################

if(BUILD_WEB_DOCS)
  include(FindUnixCommands)
  if(NOT BASH)
    message(FATAL_ERROR "Could not find bash. Unable to build documentation.")
  endif()
  if(NOT MAN2HTML)
    message(FATAL_ERROR "Could not find man2html. HTML version of man page cannot be built.")
  endif()

  add_custom_command(OUTPUT ledger.1.html
    COMMAND ${BASH} -c "man2html ${CMAKE_CURRENT_SOURCE_DIR}/ledger.1 | tail -n+3 > ledger.1.html"
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ledger.1
    VERBATIM)
  list(APPEND ledger_doc_files ledger.1.html)
endif(BUILD_WEB_DOCS)

########################################################################

add_custom_target(doc DEPENDS ${ledger_doc_files} doc.doxygen)

########################################################################

include(GNUInstallDirs)

if(CMAKE_INSTALL_MANDIR)
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/ledger.1
    DESTINATION ${CMAKE_INSTALL_MANDIR}/man1 COMPONENT doc)
endif(CMAKE_INSTALL_MANDIR)

if(CMAKE_INSTALL_DOCDIR)
  foreach(file ${info_files})
    get_filename_component(file_base ${file} NAME_WE)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${file_base}.pdf
      DESTINATION ${CMAKE_INSTALL_DOCDIR} COMPONENT doc OPTIONAL)
  endforeach()
endif(CMAKE_INSTALL_DOCDIR)
