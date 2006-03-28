#ifndef _DEBUG_H
#define _DEBUG_H

#define DEVELOPER   4
#define ALPHA       3
#define BETA        2
#define RELEASE     1
#define NO_SEATBELT 0

#ifndef DEBUG_LEVEL
#define DEBUG_LEVEL NO_SEATBELT
#endif

#if DEBUG_LEVEL >= RELEASE
#include "error.h"

#ifdef assert
#undef assert
#endif
#if DEBUG_LEVEL >= BETA
void debug_assert(const std::string& reason,
		  const std::string& file,
		  unsigned long      line);
#define assert(x)							\
  if (! (x))								\
    debug_assert(#x, __FILE__, __LINE__)
#else
#define assert(x)							\
  if (! (x))								\
    throw new fatal_assert(#x, new file_context(__FILE__, __LINE__))
#endif
#else
#ifdef assert
#undef assert
#endif
#define assert(x)
#endif

//////////////////////////////////////////////////////////////////////
//
// General debugging facilities
//
// - In developer level, all checking and debugging facilities are
//   active.
//
// - Alpha level does not include performance degrading
//   VALIDATE calls.
//
// - Beta level is like Alpha, but does not include debugging
//   facilities.
//
// - Release level does not include CONFIRM checks, but does include
//   assert calls.
//
// - Running with no seatbelt disables all checking except for normal
//   syntax and semantic error checking.

#if DEBUG_LEVEL >= ALPHA

#include <pcre.h>
#include <cstring>
#include <new>
#include <iostream>
#include <cstdlib>

#include "datetime.h"

#define DEBUG_ENABLED

extern std::ostream * _debug_stream;
extern bool           _free_debug_stream;

bool _debug_active(const char * const cls);

#define DEBUG_CLASS(cls) static const char * const _debug_cls = (cls)

#define DEBUG(cls) (_debug_active(cls))
#define DEBUG_() DEBUG(_debug_cls)

#define DEBUG_IF(cls) if (_debug_active(cls))
#define DEBUG_IF_() if (_debug_active(_debug_cls))

#define DEBUG_PRINT(cls, x)			\
  if (_debug_stream && _debug_active(cls)) {	\
    *_debug_stream << x << std::endl;		\
  }
#define DEBUG_PRINT_(x) DEBUG_PRINT(_debug_cls, x)

#define DEBUG_PRINT_TIME(cls, x) {		\
  DEBUG_PRINT(cls, #x << " is " << x);		\
}

#define DEBUG_PRINT_TIME_(x) DEBUG_PRINT_TIME(_debug_cls, x)

#define CONFIRM(x) assert(x)

#if DEBUG_LEVEL == DEVELOPER
#define VALIDATE(x) assert(x)
#else
#define VALIDATE(x)
#endif

void * operator new(std::size_t) throw (std::bad_alloc);
void * operator new[](std::size_t) throw (std::bad_alloc);
void   operator delete(void*) throw();
void   operator delete[](void*) throw();
void * operator new(std::size_t, const std::nothrow_t&) throw();
void * operator new[](std::size_t, const std::nothrow_t&) throw();
void   operator delete(void*, const std::nothrow_t&) throw();
void   operator delete[](void*, const std::nothrow_t&) throw();

#else // DEBUG_LEVEL

#define DEBUG_CLASS(cls)
#define DEBUG(cls) 0
#define DEBUG_() 0
#define DEBUG_IF(cls)
#define DEBUG_IF_()
#define DEBUG_PRINT(cls, x)
#define DEBUG_PRINT_(x)
#define DEBUG_PRINT_TIME(cls, x)
#define DEBUG_PRINT_TIME_(x)

#define VALIDATE(x)

#if DEBUG_LEVEL == NO_SEATBELT

#ifdef assert
#undef assert
#endif
#define assert(x)
#define CONFIRM(x)

#elif DEBUG_LEVEL >= RELEASE

#define CONFIRM(x)

#elif DEBUG_LEVEL >= BETA

#define CONFIRM(x) assert(x)

#endif

#endif // DEBUG_LEVEL

#endif // _DEBUG_H
