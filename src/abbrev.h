#ifndef _ABBREV_H
#define _ABBREV_H

#include "utils.h"

namespace ledger {

enum elision_style_t {
  TRUNCATE_TRAILING,
  TRUNCATE_MIDDLE,
  TRUNCATE_LEADING,
  ABBREVIATE
};

string abbreviate(const string&	  str,
		  unsigned int	  width,
		  elision_style_t elision_style = TRUNCATE_TRAILING,
		  const bool	  is_account	= false,
		  int		  abbrev_length = 2);

} // namespace ledger

#endif // _ABBREV_H
