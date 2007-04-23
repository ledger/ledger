#include "debug.h"
#include "error.h"

#ifdef DEBUG_ENABLED

#include <map>
#include <fstream>

#include <unistd.h>		// for the `write' method

int new_calls = 0;
long long new_size = 0;

void * operator new(std::size_t size) throw (std::bad_alloc) {
  void * ptr = std::malloc(size);
  new_calls++;
  new_size += size;
  return ptr;
}
void * operator new[](std::size_t size) throw (std::bad_alloc) {
  void * ptr = std::malloc(size);
  new_calls++;
  new_size += size;
  return ptr;
}
void * operator new(std::size_t size, const std::nothrow_t&) throw() {
  void * ptr = std::malloc(size);
  new_calls++;
  new_size += size;
  return ptr;
}
void * operator new[](std::size_t size, const std::nothrow_t&) throw() {
  void * ptr = std::malloc(size);
  new_calls++;
  new_size += size;
  return ptr;
}
void   operator delete(void * ptr) throw() {
  std::free(ptr);
}
void   operator delete[](void * ptr) throw() {
  std::free(ptr);
}
void   operator delete(void * ptr, const std::nothrow_t&) throw() {
  std::free(ptr);
}
void   operator delete[](void * ptr, const std::nothrow_t&) throw() {
  std::free(ptr);
}

std::ostream * _debug_stream	  = &std::cerr;
bool	       _free_debug_stream = false;
boost::regex   _debug_regex;
bool           _set_debug_regex   = false;
bool           _debug_regex_on    = false;

bool _debug_active(const char * const cls) {
  if (! _set_debug_regex) {
    const char * user_class = std::getenv("DEBUG_CLASS");
    if (user_class) {
      _debug_regex = user_class;
      _debug_regex_on = true;
    }
    _set_debug_regex = true;
  }
  if (_debug_regex_on)
    return boost::regex_match(cls, _debug_regex);
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

void debug_assert(const ledger::string& reason,
		  const ledger::string& file,
		  unsigned long		line)
{
  throw new ledger::fatal_assert(reason, new ledger::file_context(file, line));
}

#endif
