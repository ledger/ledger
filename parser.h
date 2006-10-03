#ifndef _PARSER_H
#define _PARSER_H

#include "error.h"

#include <string>
#include <istream>

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
			     const std::string * original_file = NULL) = 0;
};

class parse_error : public error {
 public:
  parse_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~parse_error() throw() {}
};

} // namespace ledger

#endif // _PARSER_H
