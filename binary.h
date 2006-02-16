#ifndef _BINARY_H
#define _BINARY_H

#include "journal.h"
#include "parser.h"

namespace ledger {

class binary_parser_t : public parser_t
{
 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream&	 in,
			     config_t&           config,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const std::string * original_file = NULL);
};

void write_binary_journal(std::ostream&	 out,
			  journal_t *	 journal);

} // namespace ledger

#endif // _BINARY_H
