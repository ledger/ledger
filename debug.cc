#include "debug.h"

#ifdef DEBUG_ENABLED

#include <map>
#include <fstream>
#include <cstdlib>
#include <cstring>

int offset = 0;

std::map<void *, int> ptrs;

#define PRINT_INC(x) {					\
  char buf[128];					\
  std::sprintf(buf, "%d: %p: %s", ++offset, ptr, x);	\
  write(1, buf, std::strlen(buf));			\
}

#define PRINT_DEC(x) {					\
  char buf[128];					\
  std::sprintf(buf, "%d: %p: %s", --offset, ptr, x);	\
  write(1, buf, std::strlen(buf));			\
}

void * operator new(std::size_t size) throw (std::bad_alloc) {
  void * ptr = std::malloc(size);
  if (DEBUG("ledger.debug.alloc")) {
    PRINT_INC("void * operator new(std::size_t size) throw (std::bad_alloc)\n");
  }
  return ptr;
}
void * operator new[](std::size_t size) throw (std::bad_alloc) {
  void * ptr = std::malloc(size);
  if (DEBUG("ledger.debug.alloc")) {
    PRINT_INC("void * operator new[](std::size_t) throw (std::bad_alloc)\n");
  }
  return ptr;
}
void * operator new(std::size_t size, const std::nothrow_t&) throw() {
  void * ptr = std::malloc(size);
  if (DEBUG("ledger.debug.alloc")) {
    PRINT_INC("void * operator new(std::size_t size, const std::nothrow_t&) throw()\n");
  }
  return ptr;
}
void * operator new[](std::size_t size, const std::nothrow_t&) throw() {
  void * ptr = std::malloc(size);
  if (DEBUG("ledger.debug.alloc")) {
    PRINT_INC("void * operator new[](std::size_t size, const std::nothrow_t&) throw()\n");
  }
  return ptr;
}
void   operator delete(void * ptr) throw() {
  if (DEBUG("ledger.debug.alloc")) {
    PRINT_DEC("void   operator delete(void * ptr) throw()\n");
  }
  std::free(ptr);
}
void   operator delete[](void * ptr) throw() {
  if (DEBUG("ledger.debug.alloc")) {
    PRINT_DEC("void   operator delete[](void * ptr) throw()\n");
  }
  std::free(ptr);
}
void   operator delete(void * ptr, const std::nothrow_t&) throw() {
  if (DEBUG("ledger.debug.alloc")) {
    PRINT_DEC("void   operator delete(void * ptr, const std::nothrow_t&) throw()\n");
  }
  std::free(ptr);
}
void   operator delete[](void * ptr, const std::nothrow_t&) throw() {
  if (DEBUG("ledger.debug.alloc")) {
    PRINT_DEC("void   operator delete[](void * ptr, const std::nothrow_t&) throw()\n");
  }
  std::free(ptr);
}

namespace ledger {

std::ostream * debug_stream	 = &std::cerr;
bool	       free_debug_stream = false;

bool _debug_active(const char * const cls) {
  if (char * debug = std::getenv("DEBUG_CLASS")) {
    static const char * error;
    static int	  erroffset;
    static int	  ovec[30];
    static pcre * class_regexp = pcre_compile(debug, PCRE_CASELESS,
					      &error, &erroffset, NULL);
    return pcre_exec(class_regexp, NULL, cls, std::strlen(cls),
		     0, 0, ovec, 30) >= 0;
  }
  return false;
}

static struct init_streams {
  init_streams() {
    // If debugging is enabled and DEBUG_FILE is set, all debugging
    // output goes to that file.
    if (const char * p = std::getenv("DEBUG_FILE")) {
      debug_stream      = new std::ofstream(p);
      free_debug_stream = true;
    }
  }
  ~init_streams() {
    if (free_debug_stream && debug_stream) {
      delete debug_stream;
      debug_stream = NULL;
    }
  }
} _debug_init;

} // namespace ledger

#endif // DEBUG_ENABLED
