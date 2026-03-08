# cmake/Packaging.cmake - CPack binary and source package configuration
#
# This module configures CPack to produce:
#   - Source tarballs (.tar.bz2) on all platforms
#   - Debian packages (.deb) on Debian/Ubuntu when dpkg is present
#   - RPM packages (.rpm) on Fedora/RHEL/openSUSE when rpmbuild is present
#
# Usage (in CMakeLists.txt):
#   include(cmake/Packaging.cmake)
#   include(CPack)

# ── Common package metadata ────────────────────────────────────────────────

set(CPACK_PACKAGE_NAME    "ledger")
set(CPACK_PACKAGE_CONTACT "John Wiegley <jwiegley@gmail.com>")

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY
  "Command-line double-entry accounting system")

set(CPACK_PACKAGE_DESCRIPTION
  "Ledger is a powerful, double-entry accounting system accessed from the
UNIX command-line.  It reads plain-text journal files and produces
financial reports; there is no database or other stored state.
Ledger is simple in concept, surprisingly rich in ability, and easy to
use once you are familiar with the journal format.")

set(CPACK_PACKAGE_HOMEPAGE_URL "https://ledger-cli.org/")

set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/LICENSE.md")

set(CPACK_PACKAGE_VERSION_MAJOR "${Ledger_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${Ledger_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH
  "${Ledger_VERSION_PATCH}${Ledger_VERSION_PRERELEASE}")

# ── Source package ─────────────────────────────────────────────────────────

set(CPACK_SOURCE_GENERATOR "TBZ2")
set(CPACK_SOURCE_PACKAGE_FILE_NAME
  "${CMAKE_PROJECT_NAME}-${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_SOURCE_IGNORE_FILES
  "/.git*;/.dir-locals.el;~$;/doc/website/;/doc/wiki/;/lib/*.sh;/lib/Makefile;/tools/;${CPACK_SOURCE_IGNORE_FILES}")

# ── Binary packages ────────────────────────────────────────────────────────

# Start with a compressed tarball as the baseline generator
set(CPACK_GENERATOR "TBZ2")

if(UNIX AND NOT APPLE)
  # ── Debian / Ubuntu (.deb) ───────────────────────────────────────────────
  # Add the DEB generator when dpkg is present on the build host.
  find_program(DPKG_PROGRAM dpkg)
  if(DPKG_PROGRAM)
    list(APPEND CPACK_GENERATOR "DEB")

    # Let dpkg-shlibdeps resolve the exact shared-library dependencies
    # automatically; this is more accurate than hard-coding version numbers.
    set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)

    # Metadata
    set(CPACK_DEBIAN_PACKAGE_SECTION  "utils")
    set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://ledger-cli.org/")

    # Use standard Debian file-name convention: <name>_<ver>_<arch>.deb
    set(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)
  endif()

  # ── Fedora / RHEL / openSUSE (.rpm) ──────────────────────────────────────
  # Add the RPM generator when rpmbuild is present on the build host.
  find_program(RPMBUILD_PROGRAM rpmbuild)
  if(RPMBUILD_PROGRAM)
    list(APPEND CPACK_GENERATOR "RPM")

    set(CPACK_RPM_PACKAGE_LICENSE "BSD-3-Clause")
    set(CPACK_RPM_PACKAGE_GROUP   "Applications/Finance")

    # Auto-detect RPM dependencies from shared libraries
    set(CPACK_RPM_PACKAGE_AUTOREQPROV ON)

    # Use standard RPM file-name convention: <name>-<ver>-<rel>.<arch>.rpm
    set(CPACK_RPM_FILE_NAME RPM-DEFAULT)
  endif()
endif()
