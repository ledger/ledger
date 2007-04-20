#include "debug.h"
#include "error.h"

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
boost::regex   _debug_regex;
bool           _set_debug_regex   = false;

bool _debug_active(const char * const cls) {
  if (! _set_debug_regex) {
    _debug_regex = std::getenv("DEBUG_CLASS");
    _set_debug_regex = true;
  }
  return boost::regex_match(cls, _debug_regex);
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

void debug_assert(const ledger::string& reason,
		  const ledger::string& file,
		  unsigned long		line)
{
  throw new ledger::fatal_assert(reason, new ledger::file_context(file, line));
}

#endif
