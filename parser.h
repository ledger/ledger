#ifndef _PARSER_H
#define _PARSER_H

#include "ledger.h"

namespace ledger {

class parser_t;
typedef std::list<parser_t *> parsers_list;

class parser_t
{
 public:
  virtual ~parser_t() {}

  virtual bool test(std::istream& in) const = 0;

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const std::string * original_file = NULL) = 0;

  static parsers_list parsers;

  static unsigned int parse_file(const std::string&  path,
				 journal_t *	     journal,
				 account_t *	     master	   = NULL,
				 const std::string * original_file = NULL);
};

} // namespace ledger

#endif // _PARSER_H
