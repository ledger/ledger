#include "mask.h"

namespace ledger {

mask_t::mask_t(const string& pat) : exclude(false)
{
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

  expr.assign(p);
}

} // namespace ledger
