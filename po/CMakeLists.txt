if (GETTEXT_FOUND)

include(GNUInstallDirs)
if(NOT CMAKE_INSTALL_LOCALEDIR)
  MESSAGE(STATUS "CMAKE_INSTALL_LOCALEDIR not found, i18n files will not be installed")
else()

set(potfile "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pot")
file(GLOB potfiles_in
  RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}/../src/"
  "${CMAKE_CURRENT_SOURCE_DIR}/../src/*.h"
  "${CMAKE_CURRENT_SOURCE_DIR}/../src/*.cc")

add_custom_command(OUTPUT ${potfile}
  COMMAND xgettext
          --default-domain=${PROJECT_NAME}
          --package-name=${PROJECT_NAME}
          --package-version=${PROJECT_VERSION}
          --copyright-holder="John Wiegley"
          --msgid-bugs-address="https://bugs.ledger-cli.org"
          --keyword='_'
          --keyword='_f'
          --keyword='_n:1,2'
          --keyword='_fn:1,2'
          --keyword='proper_name:1,"This is a proper name. See the gettext manual, section Names."'
          --add-comments=TRANSLATORS:
          --c++
          --boost
          --no-wrap
          --sort-by-file
          --from-code=UTF-8
          --output ${potfile}
          --directory "${CMAKE_CURRENT_SOURCE_DIR}/../src"
          ${potfiles_in}
          )

file(GLOB pofiles "${CMAKE_CURRENT_SOURCE_DIR}/*.po")
GETTEXT_CREATE_TRANSLATIONS(${potfile} ALL ${pofiles})

endif(NOT CMAKE_INSTALL_LOCALEDIR)

endif(GETTEXT_FOUND)
