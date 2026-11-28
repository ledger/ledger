# utfcpp (vendored)

This directory holds the part of the header-only
[utfcpp](https://github.com/nemtrif/utfcpp) library, version 4.0.9, that
`#include "utf8.h"` (src/system.hh.in) reaches: `v4/source/utf8.h` and the six
headers below `v4/source/utf8/`, copied verbatim from upstream's `source/`
directory.  Upstream's tests, CI workflow, CMake packaging and README are left
out; to update, replace `v4/source/` with the `source/` directory of the new
release and `v4/LICENSE` with its `LICENSE`.  It is distributed under the Boost
Software License 1.0 (see `v4/LICENSE`).

Ledger uses utfcpp to convert between UTF-8 and UTF-32 (src/unistring.h,
src/mask.cc, src/textual.cc, src/textual_directives.cc, src/py_utils.cc).
`cmake/FindUtfcpp.cmake` prefers a system installation (hint:
`-DUTFCPP_PATH=...`) and falls back to this copy.
