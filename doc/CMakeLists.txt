# The following will be generated or updated when the 'doc' target is built:
# • user guide and man pages: if BUILD_DOCS is set
# • HTML versions of the above: if BUILD_DOCS and BUILD_WEB_DOCS are set
# • Doxygen / reference documentation: if USE_DOXYGEN is set

########################################################################

set(Required_CMake_Version 3.16.2)
set(Required_Doxygen_Version 1.9.5)

cmake_minimum_required(VERSION ${Required_CMake_Version})

project(ledger NONE)

set(DOXYGEN_EXTRA_CSS CACHE STRING
  "Path to extra css file added to the generated API documentation")
set(DOXYGEN_HTML_HEADER CACHE STRING
  "Path to html file used as the generated API documentation header")

# Point CMake at any custom modules we may ship
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
include(LedgerVersion)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/version.texi.in
  ${CMAKE_CURRENT_SOURCE_DIR}/version.texi)

if (USE_DOXYGEN)
  find_package(Doxygen ${Required_Doxygen_Version})
  if (NOT DOXYGEN_FOUND)
    message(FATAL_ERROR "Could not find doxygen. Reference documentation cannot be built.")
  endif()

  set(DOXYGEN_PROJECT_NUMBER "${Ledger_VERSION_MAJOR}.${Ledger_VERSION_MINOR}.${Ledger_VERSION_PATCH}")
  set(DOXYGEN_DOT_IMAGE_FORMAT svg)
  set(DOXYGEN_ALWAYS_DETAILED_SEC YES)
  set(DOXYGEN_INLINE_INHERITED_MEMB YES)
  set(DOXYGEN_FULL_PATH_NAMES NO)
  set(DOXYGEN_FILE_PATTERNS "*.h")
  set(DOXYGEN_SHORT_NAMES YES)
  set(DOXYGEN_JAVADOC_AUTOBRIEF YES)
  set(DOXYGEN_TAB_SIZE 8)
  set(DOXYGEN_BUILTIN_STL_SUPPORT YES)
  set(DOXYGEN_EXTRACT_ALL YES)
  set(DOXYGEN_EXTRACT_LOCAL_CLASSES NO)
  set(DOXYGEN_HIDE_UNDOC_CLASSES YES)
  set(DOXYGEN_INTERNAL_DOCS YES)
  set(DOXYGEN_CASE_SENSE_NAMES NO)
  set(DOXYGEN_SORT_BRIEF_DOCS YES)
  set(DOXYGEN_WARN_IF_UNDOCUMENTED NO) # TODO: set to YES
  set(DOXYGEN_WARN_NO_PARAMDOC YES)
  set(DOXYGEN_RECURSIVE NO)
  set(DOXYGEN_MACRO_EXPANSION YES)
  set(DOXYGEN_SOURCE_BROWSER YES)
  set(DOXYGEN_GENERATE_TREEVIEW  YES)
  set(DOXYGEN_REFERENCED_BY_RELATION YES)
  set(DOXYGEN_REFERENCES_RELATION YES)
  set(DOXYGEN_HTML_EXTRA_STYLESHEET "${CMAKE_CURRENT_SOURCE_DIR}/apidoc.css" "${DOXYGEN_EXTRA_CSS}")
  set(DOXYGEN_STRIP_FROM_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../src/")
  set(DOXYGEN_HTML_TIMESTAMP YES)
  set(DOXYGEN_HTML_DYNAMIC_SECTIONS YES)
  set(DOXYGEN_TEMPLATE_RELATIONS YES)
  set(DOXYGEN_CALL_GRAPH YES)
  set(DOXYGEN_CALLER_GRAPH YES)
  set(DOXYGEN_MAX_DOT_GRAPH_DEPTH 1000)
  set(DOXYGEN_DOT_MULTI_TARGETS NO)

  doxygen_add_docs(doc.doxygen
    "${CMAKE_CURRENT_SOURCE_DIR}/../src" "${CMAKE_CURRENT_SOURCE_DIR}/mainpage.txt"
    COMMENT "Generating API documentation")

  if(CMAKE_INSTALL_DOCDIR)
    install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html
      DESTINATION ${CMAKE_INSTALL_DOCDIR} COMPONENT doc)
  endif()
else()
  add_custom_target(doc.doxygen)
endif()

########################################################################

# BUILD_WEB_DOCS implies BUILD_DOCS
if (BUILD_WEB_DOCS)
  set(BUILD_DOCS 1)
endif()

if (BUILD_DOCS)
  find_program(MAKEINFO makeinfo)
  find_program(TEXI2PDF texi2pdf)
  find_program(TEX tex)
  find_program(MAN2HTML man2html)
  find_program(GROFF groff)
  set(ledger_info_files ledger3.texi)

  if (NOT MAKEINFO)
    message(WARNING "Could not find makeinfo. Info version of documentation cannot be built.")
  endif()

  if (NOT TEXI2PDF OR NOT TEX)
    message(WARNING "Could not find texi2pdf or tex. PDF version of documentation will not be built.")
  endif()
endif()

########################################################################

foreach(file ${ledger_info_files})
  get_filename_component(file_base ${file} NAME_WE)

  if (MAKEINFO)
    add_custom_command(OUTPUT ${file_base}.info
      COMMAND makeinfo --force --no-split -o ${file_base}.info ${CMAKE_CURRENT_SOURCE_DIR}/${file}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
      VERBATIM)
    list(APPEND ledger_doc_files ${file_base}.info)
  endif()

  if (BUILD_WEB_DOCS AND MAKEINFO)
    add_custom_command(OUTPUT ${file_base}.html
      COMMAND makeinfo --force --html --no-split -o ${file_base}.html ${CMAKE_CURRENT_SOURCE_DIR}/${file}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
      VERBATIM)
    list(APPEND ledger_doc_files ${file_base}.html)
  endif()

  if (TEXI2PDF AND TEX)
    if (BUILD_A4_PDF)
      set(papersize --texinfo=@afourpaper)
    endif()
    add_custom_command(OUTPUT ${file_base}.pdf
      COMMAND texi2pdf ${papersize} -b -q -o ${file_base}.pdf ${CMAKE_CURRENT_SOURCE_DIR}/${file}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}
      VERBATIM)
    list(APPEND ledger_doc_files ${file_base}.pdf)
  endif()
endforeach()

########################################################################

if (BUILD_WEB_DOCS)
  include(FindUnixCommands)
  if (NOT BASH)
    message(FATAL_ERROR "Could not find bash. Unable to build documentation.")
  endif()
  if (MAN2HTML)
    add_custom_command(OUTPUT ledger.1.html
      COMMAND ${BASH} -c "man2html ${CMAKE_CURRENT_SOURCE_DIR}/ledger.1 | tail -n+3 > ledger.1.html"
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ledger.1
      VERBATIM)
    list(APPEND ledger_doc_files ledger.1.html)
  elseif(GROFF)
    add_custom_command(OUTPUT ledger.1.html
      COMMAND ${BASH} -c "groff -mandoc -Thtml ${CMAKE_CURRENT_SOURCE_DIR}/ledger.1 > ledger.1.html"
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ledger.1
      VERBATIM)
    list(APPEND ledger_doc_files ledger.1.html)
  else()
    message(FATAL_ERROR "Could not find man2html or groff. HTML version of man page cannot be built.")
  endif()
endif(BUILD_WEB_DOCS)

########################################################################

add_custom_target(doc DEPENDS ${ledger_doc_files} doc.doxygen)

########################################################################

include(GNUInstallDirs)

if (CMAKE_INSTALL_MANDIR)
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/ledger.1
    DESTINATION ${CMAKE_INSTALL_MANDIR}/man1 COMPONENT doc)
endif(CMAKE_INSTALL_MANDIR)

foreach(file ${ledger_doc_files})
  get_filename_component(file_ext ${file} EXT)

  if (file_ext STREQUAL ".info")
    if (CMAKE_INSTALL_INFODIR)
      install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${file}
        DESTINATION ${CMAKE_INSTALL_INFODIR} COMPONENT doc)
    endif()
  elseif(CMAKE_INSTALL_DOCDIR)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${file}
      DESTINATION ${CMAKE_INSTALL_DOCDIR} COMPONENT doc)
  endif()
endforeach()
