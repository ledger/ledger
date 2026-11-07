# - Try to find immer (persistent, structurally shared data structures)
# Once done, this will define
#
#  IMMER_FOUND        - immer/map.hpp was found
#  IMMER_INCLUDE_DIR  - the include directory holding immer/
#
# immer is header-only.  A system installation is preferred (hint it with
# IMMER_PATH); the copy vendored below lib/immer/ is the fallback.

set(IMMER_FOUND FALSE)

find_path(IMMER_INCLUDE_DIR
    NAMES immer/map.hpp
    HINTS "${IMMER_PATH}"
    PATHS "${PROJECT_SOURCE_DIR}/lib/immer"
)

if (IMMER_INCLUDE_DIR)
  set(IMMER_FOUND TRUE)
endif()
