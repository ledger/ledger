#ifndef _PARSER_H
#define _PARSER_H

#include <iostream>
#include <string>

#include "error.h"

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

bool register_parser(parser_t * parser);
bool unregister_parser(parser_t * parser);

unsigned int parse_journal(std::istream&       in,
			   journal_t *	       journal,
			   account_t *	       master        = NULL,
			   const std::string * original_file = NULL);

unsigned int parse_journal_file(const std::string&  path,
				journal_t *	    journal,
				account_t *	    master        = NULL,
				const std::string * original_file = NULL);

void initialize_parser_support();
void shutdown_parser_support();

class parse_error : public error {
 public:
  parse_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~parse_error() throw() {}
};

} // namespace ledger

#endif // _PARSER_H
