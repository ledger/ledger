set(EMACS_LISP_SOURCES
  ldg-commodities.el
  ldg-complete.el
  ldg-exec.el
  ldg-fonts.el
  ldg-init.el
  ldg-mode.el
  ldg-new.el
  ldg-occur.el
  ldg-post.el
  ldg-reconcile.el
  ldg-regex.el
  ldg-report.el
  ldg-sort.el
  ldg-state.el
  ldg-test.el
  ldg-texi.el
  ldg-xact.el
  ldg-context.el
  ldg-schedule.el)

# find emacs and complain if not found
find_program(EMACS_EXECUTABLE emacs)

macro(add_emacs_lisp_target el)
  configure_file(${el} ${CMAKE_CURRENT_BINARY_DIR}/${el})

  # add rule (i.e. command) how to generate the byte-compiled file
  add_custom_command(
    OUTPUT ${el}c
    COMMAND ${EMACS_EXECUTABLE}
            -L ${CMAKE_CURRENT_BINARY_DIR}
            -l ${CMAKE_CURRENT_BINARY_DIR}/ldg-regex.el
            -batch -f batch-byte-compile
             ${CMAKE_CURRENT_BINARY_DIR}/${el}
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${el}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Creating byte-compiled Emacs lisp ${CMAKE_CURRENT_BINARY_DIR}/${el}c")
endmacro(add_emacs_lisp_target el)

if(EMACS_EXECUTABLE)
  foreach(el ${EMACS_LISP_SOURCES})
    add_emacs_lisp_target(${el})
    list(APPEND EMACS_LISP_BINARIES ${CMAKE_CURRENT_BINARY_DIR}/${el}c)
  endforeach()

  add_custom_target(emacs_lisp_byte_compile ALL DEPENDS ${EMACS_LISP_BINARIES})

  # install the byte-compiled emacs-lisp sources
  install(FILES ${EMACS_LISP_SOURCES} ${EMACS_LISP_BINARIES}
    DESTINATION share/emacs/site-lisp)
endif()

### CMakeLists.txt ends here
