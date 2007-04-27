#ifndef _PARSER_H
#define _PARSER_H

#include "utils.h"

namespace ledger {

class account_t;
class journal_t;

class parser_t
{
 public:
  virtual ~parser_t() {}

  virtual bool test(std::istream& in) const = 0;

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const string * original_file = NULL) = 0;
};

DEFINE_EXCEPTION(parse_exception)

} // namespace ledger

#endif // _PARSER_H
