#ifdef USE_PCH
#include "pch.h"
#else
#include "mask.h"
#include "debug.h"
#include "util.h"

#include <cstdlib>

#include <pcre.h>
#endif

mask_t::mask_t(const std::string& pat) : exclude(false)
{
  TRACE_CTOR("mask_t(const std::string&)");

  const char * p = pat.c_str();
  if (*p == '-') {
    exclude = true;
    p++;
    while (std::isspace(*p))
      p++;
  }
  else if (*p == '+') {
    p++;
    while (std::isspace(*p))
      p++;
  }
  pattern = p;

  const char *error;
  int erroffset;
  regexp = pcre_compile(pattern.c_str(), PCRE_CASELESS,
			&error, &erroffset, NULL);
  if (! regexp)
    throw new mask_error(std::string("Failed to compile regexp '") +
			 pattern + "'");
}

mask_t::mask_t(const mask_t& m) : exclude(m.exclude), pattern(m.pattern)
{
  TRACE_CTOR("mask_t(copy)");

  const char *error;
  int erroffset;
  regexp = pcre_compile(pattern.c_str(), PCRE_CASELESS,
			&error, &erroffset, NULL);
  assert(regexp);
}

mask_t::~mask_t() {
  TRACE_DTOR("mask_t");
  pcre_free((pcre *)regexp);
}

bool mask_t::match(const std::string& str) const
{
  static int ovec[30];
  int result = pcre_exec((pcre *)regexp, NULL,
			 str.c_str(), str.length(), 0, 0, ovec, 30);
  return result >= 0 && ! exclude;
}
