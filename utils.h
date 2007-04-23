#ifndef _UTILS_H
#define _UTILS_H

#include <system.hh>

// jww (2007-04-23): Need to clean up the following include files.  I
// want to following services:
//
//   error reporting via exceptions
//   error context stack and display (copy-by-value)
//   logging (always on, but with user-settable levels)
//   assert (always on, unless the users asks for them off)
//   timing of critical areas (and warning on variance from expectation)
//   debugging (optionally on)
//   verification (optionally on, like debugging but silent)
//   memory tracing and debugging (and watching for threshholds)

#include "trace.h"
#include "debug.h"
#include "timing.h"
#include "error.h"
#include "util.h"

#endif // _UTILS_H
