#include "debug.h"

#ifdef DEBUG_ENABLED

#include <map>
#include <fstream>

#include <unistd.h>		// for the `write' method

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
#if 0				// jww (2007-04-19): these don't work with boost::regex
  if (DEBUG("debug.alloc")) {
    PRINT_INC("void * operator new(std::size_t size) throw (std::bad_alloc)\n");
  }
#endif
  return ptr;
}
void * operator new[](std::size_t size) throw (std::bad_alloc) {
  void * ptr = std::malloc(size);
#if 0
  if (DEBUG("debug.alloc")) {
    PRINT_INC("void * operator new[](std::size_t) throw (std::bad_alloc)\n");
  }
#endif
  return ptr;
}
void * operator new(std::size_t size, const std::nothrow_t&) throw() {
  void * ptr = std::malloc(size);
#if 0
  if (DEBUG("debug.alloc")) {
    PRINT_INC("void * operator new(std::size_t size, const std::nothrow_t&) throw()\n");
  }
#endif
  return ptr;
}
void * operator new[](std::size_t size, const std::nothrow_t&) throw() {
  void * ptr = std::malloc(size);
#if 0
  if (DEBUG("debug.alloc")) {
    PRINT_INC("void * operator new[](std::size_t size, const std::nothrow_t&) throw()\n");
  }
#endif
  return ptr;
}
void   operator delete(void * ptr) throw() {
#if 0
  if (DEBUG("debug.alloc")) {
    PRINT_DEC("void   operator delete(void * ptr) throw()\n");
  }
#endif
  std::free(ptr);
}
void   operator delete[](void * ptr) throw() {
#if 0
  if (DEBUG("debug.alloc")) {
    PRINT_DEC("void   operator delete[](void * ptr) throw()\n");
  }
#endif
  std::free(ptr);
}
void   operator delete(void * ptr, const std::nothrow_t&) throw() {
#if 0
  if (DEBUG("debug.alloc")) {
    PRINT_DEC("void   operator delete(void * ptr, const std::nothrow_t&) throw()\n");
  }
#endif
  std::free(ptr);
}
void   operator delete[](void * ptr, const std::nothrow_t&) throw() {
#if 0
  if (DEBUG("debug.alloc")) {
    PRINT_DEC("void   operator delete[](void * ptr, const std::nothrow_t&) throw()\n");
  }
#endif
  std::free(ptr);
}

std::ostream * _debug_stream	  = &std::cerr;
bool	       _free_debug_stream = false;

bool _debug_active(const char * const cls) {
  if (char * debug = std::getenv("DEBUG_CLASS")) {
    return boost::regex_match(cls, boost::regex(debug));
  }
  return false;
}

static struct init_streams {
  init_streams() {
    // If debugging is enabled and DEBUG_FILE is set, all debugging
    // output goes to that file.
    if (const char * p = std::getenv("DEBUG_FILE")) {
      _debug_stream      = new std::ofstream(p);
      _free_debug_stream = true;
    }
  }
  ~init_streams() {
    if (_free_debug_stream && _debug_stream) {
      delete _debug_stream;
      _debug_stream = NULL;
    }
  }
} _debug_init;

#endif // DEBUG_ENABLED

#if DEBUG_LEVEL >= BETA

#include <string>

void debug_assert(const std::string& reason,
		  const std::string& file,
		  unsigned long      line)
{
  throw new fatal_assert(reason, new file_context(file, line));
}

#endif
