#include "debug.h"

#ifdef DEBUG_ENABLED

namespace ledger {

std::ostream * debug_stream	 = &std::cerr;
bool	       free_debug_stream = false;

static class free_streams
{
 public:
  ~free_streams() {
    if (free_debug_stream && debug_stream) {
      delete debug_stream;
      debug_stream = NULL;
    }
  }
} _debug_cleanup;

} // namespace ledger

#endif DEBUG_ENABLED
