#include "debug.h"

#ifdef DEBUG_ENABLED

#include <fstream>
#include <cstdlib>

namespace ledger {

std::ostream * debug_stream	 = &std::cerr;
bool	       free_debug_stream = false;

static struct init_streams {
  init_streams() {
    // If debugging is enabled and DEBUG_FILE is set, all debugging
    // output goes to that file.
    if (const char * p = std::getenv("DEBUG_FILE")) {
      debug_stream      = new std::ofstream(p);
      free_debug_stream = true;
    }
  }
} _debug_init;

static struct free_streams {
  ~free_streams() {
    if (free_debug_stream && debug_stream) {
      delete debug_stream;
      debug_stream = NULL;
    }
  }
} _debug_cleanup;

} // namespace ledger

#endif DEBUG_ENABLED
