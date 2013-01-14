set(info_files ledger.texi ledger3.texi)

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
      COMMAND makeinfo --force --html --no-split -o ${file_base}.html ${file}
      DEPENDS ${file}
      VERBATIM)
    list(APPEND ledger_doc_files ${file_base}.html)
  endif(BUILD_WEB_DOCS)

  if(NOT TEXI2PDF)
    mesage(WARNING "Could not find texi2pdf. PDF version of documentation will not be built.")
  else()
    get_filename_component(file_base ${file} NAME_WE)
    add_custom_command(OUTPUT ${file_base}.pdf
      COMMAND texi2pdf -b -q ${file}
      COMMAND rm -f ${file_base}.aux ${file_base}.cp ${file_base}.fn ${file_base}.ky ${file_base}.log ${file_base}.pg ${file_base}.toc ${file_base}.tp ${file_base}.vr
      DEPENDS ${file}
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
    COMMAND ${BASH} -c "man2html ledger.1 | tail -n+3 > ledger.1.html"
    DEPENDS ledger.1
    VERBATIM)
  list(APPEND ledger_doc_files ledger.1.html)
endif(BUILD_WEB_DOCS)

########################################################################

add_custom_target(doc ALL DEPENDS ${ledger_doc_files})

########################################################################

include(GNUInstallDirs)

if(CMAKE_INSTALL_MANDIR)
  install(FILES ledger.1
    DESTINATION ${CMAKE_INSTALL_MANDIR}/man1 COMPONENT doc)
endif(CMAKE_INSTALL_MANDIR)

if(CMAKE_INSTALL_DOCDIR)
  foreach(file ${info_files})
    get_filename_component(file_base ${file} NAME_WE)
    install(FILES ${file_base}.pdf
      DESTINATION ${CMAKE_INSTALL_DOCDIR} COMPONENT doc OPTIONAL)
  endforeach()
endif(CMAKE_INSTALL_DOCDIR)
