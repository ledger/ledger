#ifndef _DEBUG_H
#define _DEBUG_H

#define DEVELOPER   4
#define ALPHA       3
#define BETA        2
#define RELEASE     1
#define NO_SEATBELT 0

#ifndef RELEASE_LEVEL
#define RELEASE_LEVEL RELEASE
#endif

#if RELEASE_LEVEL >= ALPHA
#include <cstdlib>              // for `getenv'
#include <cstring>              // for `strcmp'
#endif

#if RELEASE_LEVEL >= BETA
#include <cassert>
#endif

#if RELEASE_LEVEL >= RELEASE
#include <iostream>
#endif

#include <sstream>              // used for constructing exceptions

namespace ledger {

//////////////////////////////////////////////////////////////////////
//
// Debugging facilities for Ledger
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

#define WARN(x)							\
  if (ledger::warning_stream)					\
    *ledger::warning_stream << "Warning: "<< x << std::endl

#define ERR(exc, x) {				\
    std::ostringstream s;                       \
    s << x;                                     \
    throw exc(s.str().c_str());                 \
  }

#if RELEASE_LEVEL >= ALPHA

#define DEBUG_ENABLED

extern std::ostream * warning_stream;
extern std::ostream * debug_stream;
extern bool           free_debug_stream;

inline bool _debug_active(const char * const cls) {
  char * debug = std::getenv("DEBUG_CLASS");
  return debug && std::strcmp(debug, cls) == 0;
}

#define DEBUG_CLASS(cls) static const char * const _debug_cls = (cls)

#define DEBUG(cls) (ledger::_debug_active(cls))
#define DEBUG_() DEBUG(_debug_cls)

#define DEBUG_PRINT(cls, x)					\
  if (ledger::debug_stream && ledger::_debug_active(cls)) {	\
    *ledger::debug_stream << x << std::endl;			\
  }
#define DEBUG_PRINT_(x) DEBUG_PRINT(_debug_cls, x)

#define CONFIRM(x) assert(x)

#if RELEASE_LEVEL == DEVELOPER
#define VALIDATE(x) assert(x)
#else
#define VALIDATE(x)
#endif

#else // RELEASE_LEVEL

#define DEBUG_CLASS(cls)
#define DEBUG(cls) 0
#define DEBUG_() 0
#define DEBUG_PRINT(cls, x)
#define DEBUG_PRINT_(x)

#define VALIDATE(x)

#if RELEASE_LEVEL == NO_SEATBELT

#undef assert
#define assert(x)
#define CONFIRM(x)

#elif RELEASE_LEVEL >= RELEASE

extern std::ostream * warning_stream;
#define CONFIRM(x)

#elif RELEASE_LEVEL >= BETA

#define CONFIRM(x) assert(x)

#endif

#endif // RELEASE_LEVEL

} // namespace ledger

#endif // _DEBUG_H
