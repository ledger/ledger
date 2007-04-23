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

class parse_error : public error {
 public:
  parse_error(const string& _reason,
	      error_context * _ctxt = NULL) throw()
    : error(_reason, _ctxt) {}
  virtual ~parse_error() throw() {}
};

} // namespace ledger

#endif // _PARSER_H
